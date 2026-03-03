%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for actors, inspect, kill, and interrupt operations.
%%%
%%% **DDD Context:** REPL
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_actors).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, validate_actor_pid/1, is_known_actor/1]).

%% @private
-spec encode_invalid_pid_error(atom(), string(), beamtalk_repl_protocol:protocol_msg()) -> binary().
encode_invalid_pid_error(Reason, PidStr, Msg) ->
    PidBin = list_to_binary(PidStr),
    Err0 = beamtalk_error:new(Reason, 'Actor'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Invalid actor PID: ">>, PidBin])
    ),
    Err2 = beamtalk_error:with_hint(
        Err1,
        <<"Use :actors to list valid actor PIDs.">>
    ),
    beamtalk_repl_protocol:encode_error(
        Err2, Msg, fun beamtalk_repl_json:format_error_message/1
    ).

%% @doc Handle actors/inspect/kill/interrupt ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"actors">>, _Params, Msg, _SessionPid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            beamtalk_repl_protocol:encode_actors([], Msg, fun beamtalk_repl_json:term_to_json/1);
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            beamtalk_repl_protocol:encode_actors(Actors, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;
handle(<<"inspect">>, Params, Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, <<>>)),
    PidBin = list_to_binary(PidStr),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            encode_invalid_pid_error(Reason, PidStr, Msg);
        {ok, Pid} ->
            case is_process_alive(Pid) of
                true ->
                    try
                        State = sys:get_state(Pid, 5000),
                        case State of
                            M when is_map(M) ->
                                case beamtalk_tagged_map:is_tagged(M) of
                                    true ->
                                        %% Return only user-visible instance fields
                                        %% (filters out $beamtalk_class, __methods__, etc.)
                                        UserFields = beamtalk_reflection:field_names(M),
                                        FieldMap = maps:with(UserFields, M),
                                        beamtalk_repl_protocol:encode_inspect(
                                            FieldMap, Msg, fun beamtalk_repl_json:term_to_json/1
                                        );
                                    false ->
                                        beamtalk_repl_protocol:encode_inspect(
                                            M, Msg, fun beamtalk_repl_json:term_to_json/1
                                        )
                                end;
                            _ ->
                                InspectStr = iolist_to_binary(io_lib:format("~p", [State])),
                                beamtalk_repl_protocol:encode_inspect(InspectStr, Msg)
                        end
                    catch
                        _:_ ->
                            Err3 = beamtalk_error:new(inspect_failed, 'Actor'),
                            Err4 = beamtalk_error:with_message(
                                Err3,
                                iolist_to_binary([<<"Failed to inspect actor: ">>, PidBin])
                            ),
                            beamtalk_repl_protocol:encode_error(
                                Err4, Msg, fun beamtalk_repl_json:format_error_message/1
                            )
                    end;
                false ->
                    Err3 = beamtalk_error:new(actor_not_alive, 'Actor'),
                    Err4 = beamtalk_error:with_message(
                        Err3,
                        iolist_to_binary([<<"Actor is not alive: ">>, PidBin])
                    ),
                    beamtalk_repl_protocol:encode_error(
                        Err4, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"kill">>, Params, Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, maps:get(<<"pid">>, Params, <<>>))),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            encode_invalid_pid_error(Reason, PidStr, Msg);
        {ok, Pid} ->
            exit(Pid, kill),
            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;
handle(<<"interrupt">>, _Params, Msg, SessionPid) ->
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
                            session => TargetSession
                        }),
                        SessionPid
                end
        end,
    try
        ok = beamtalk_repl_shell:interrupt(TargetPid),
        beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    catch
        exit:{noproc, _} ->
            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    end.

%%% Internal helpers

%% @private
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

%% @private
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
