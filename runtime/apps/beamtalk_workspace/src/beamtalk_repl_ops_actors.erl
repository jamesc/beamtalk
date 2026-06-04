%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_actors).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for actors, inspect, kill, and interrupt operations.

Extracted from beamtalk_repl_server (BT-705). These are the actor read-surface
ops; `handle_term/4` returns native `beamtalk_repl_ops:op_result()` terms
(BT-2399, ADR 0017 Phase 3) so dist-attached clients receive live actor pids
and field maps rather than flattened JSON. `handle/4` is the WebSocket-edge
wrapper that encodes the term result via `beamtalk_repl_ops:encode/2`.
""".

-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, validate_actor_pid/1, is_known_actor/1]).

-spec invalid_pid_error(atom(), string()) -> beamtalk_error:error().
invalid_pid_error(Reason, PidStr) ->
    PidBin = list_to_binary(PidStr),
    Err0 = beamtalk_error:new(Reason, 'Actor'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Invalid actor PID: ">>, PidBin])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Use :actors to list valid actor PIDs.">>
    ).

-doc """
Handle actors/inspect/kill/interrupt ops for the WebSocket transport — encodes
the term result to JSON at the edge.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for actors/inspect/kill/interrupt. Returns
`{actors, [Meta]}`, `{inspect, map() | binary()}`, `{status, ok}`, or
`{error, #beamtalk_error{}}` — no JSON in this path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"actors">>, _Params, _Msg, _SessionPid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            {actors, []};
        RegistryPid ->
            {actors, beamtalk_repl_actors:list_actors(RegistryPid)}
    end;
handle_term(<<"inspect">>, Params, _Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, <<>>)),
    PidBin = list_to_binary(PidStr),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            {error, invalid_pid_error(Reason, PidStr)};
        {ok, Pid} ->
            case is_process_alive(Pid) of
                true ->
                    %% Keep the whole introspection inside the try body (not a
                    %% `try ... of`): is_tagged/field_names/maps:with must also be
                    %% guarded, so a failure there returns inspect_failed rather
                    %% than crashing the op handler.
                    try
                        State = sys:get_state(Pid, 5000),
                        case State of
                            M when is_map(M) ->
                                case beamtalk_runtime_api:is_tagged(M) of
                                    true ->
                                        %% Return only user-visible instance fields
                                        %% (filters out $beamtalk_class, __methods__, etc.)
                                        UserFields = beamtalk_runtime_api:field_names(M),
                                        {inspect, maps:with(UserFields, M)};
                                    false ->
                                        {inspect, M}
                                end;
                            _ ->
                                {inspect, iolist_to_binary(io_lib:format("~p", [State]))}
                        end
                    catch
                        _:_ ->
                            Err3 = beamtalk_error:new(inspect_failed, 'Actor'),
                            Err4 = beamtalk_error:with_message(
                                Err3,
                                iolist_to_binary([<<"Failed to inspect actor: ">>, PidBin])
                            ),
                            {error, Err4}
                    end;
                false ->
                    Err3 = beamtalk_error:new(actor_not_alive, 'Actor'),
                    Err4 = beamtalk_error:with_message(
                        Err3,
                        iolist_to_binary([<<"Actor is not alive: ">>, PidBin])
                    ),
                    {error, Err4}
            end
    end;
handle_term(<<"kill">>, Params, _Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, maps:get(<<"pid">>, Params, <<>>))),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            {error, invalid_pid_error(Reason, PidStr)};
        {ok, Pid} ->
            exit(Pid, kill),
            {status, ok}
    end;
handle_term(<<"interrupt">>, _Params, Msg, SessionPid) ->
    %% BT-666: Interrupt a running evaluation.
    %% BT-1045: session is stripped from Params by the protocol decoder — use get_session(Msg).
    TargetPid =
        case beamtalk_repl_protocol:get_session(Msg) of
            undefined ->
                SessionPid;
            TargetSession ->
                case beamtalk_session_table:lookup(TargetSession) of
                    {ok, Pid} ->
                        Pid;
                    error ->
                        ?LOG_WARNING("Interrupt target session not found", #{
                            session => TargetSession,
                            domain => [beamtalk, runtime]
                        }),
                        SessionPid
                end
        end,
    try
        ok = beamtalk_repl_shell:interrupt(TargetPid),
        {status, ok}
    catch
        exit:{noproc, _} ->
            {status, ok}
    end.

%%% Internal helpers

-spec validate_actor_pid(string()) -> {ok, pid()} | {error, invalid_pid | unknown_actor}.
validate_actor_pid(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case is_known_actor(Pid) of
            true -> {ok, Pid};
            false -> {error, unknown_actor}
        end
    catch
        _:_ -> {error, invalid_pid}
    end.

-spec is_known_actor(pid()) -> boolean().
is_known_actor(Pid) when is_pid(Pid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            false;
        RegistryPid ->
            case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                {ok, _} -> true;
                {error, not_found} -> false
            end
    end.
