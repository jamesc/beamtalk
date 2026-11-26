%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_actors).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for actors, inspect, kill, and interrupt operations.

Extracted from beamtalk_repl_server. These are the actor read-surface
ops; `handle_term/4` returns native `beamtalk_repl_ops:op_result()` terms
(ADR 0017 Phase 3) so dist-attached clients receive live actor pids
and field maps rather than flattened JSON. `handle/4` is the WebSocket-edge
wrapper that encodes the term result via `beamtalk_repl_ops:encode/2`.
""".

-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4]).

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
    {actors, beamtalk_workspace_actors:list()};
handle_term(<<"inspect">>, Params, _Msg, _SessionPid) ->
    case beamtalk_repl_params:actor_pid(Params) of
        {error, _} = Err ->
            Err;
        {ok, Pid, PidBin} ->
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
                                {inspect,
                                    beamtalk_error:chars_to_binary(io_lib:format("~tp", [State]))}
                        end
                    catch
                        _:_ ->
                            {error,
                                beamtalk_repl_errors:make(
                                    inspect_failed,
                                    'Actor',
                                    iolist_to_binary([<<"Failed to inspect actor: ">>, PidBin])
                                )}
                    end;
                false ->
                    {error,
                        beamtalk_repl_errors:make(
                            actor_not_alive,
                            'Actor',
                            iolist_to_binary([<<"Actor is not alive: ">>, PidBin])
                        )}
            end
    end;
handle_term(<<"kill">>, Params, _Msg, _SessionPid) ->
    case beamtalk_repl_params:actor_pid(Params) of
        {error, _} = Err ->
            Err;
        {ok, Pid, PidBin} ->
            case beamtalk_workspace_actors:lookup(Pid) of
                {ok, #{system := true, class := Class}} ->
                    %% BT-4960: killing a runtime-started actor (`Transcript`)
                    %% would drop its buffer and every subscriber.
                    {error,
                        beamtalk_repl_errors:make(
                            system_actor,
                            'Actor',
                            iolist_to_binary([
                                <<"Cannot kill a system actor: ">>,
                                PidBin,
                                <<" (">>,
                                atom_to_binary(Class, utf8),
                                <<")">>
                            ]),
                            <<"System actors are started by the runtime; use :actors to find user actors.">>
                        )};
                {error, not_found} ->
                    %% BT-5199: no longer an actor between the param check and
                    %% here — refuse rather than report a kill that never ran.
                    {error, not_killable_error(Pid, PidBin)};
                {ok, _} ->
                    kill_and_wait(Pid),
                    {status, ok}
            end
    end;
handle_term(<<"interrupt">>, _Params, Msg, SessionPid) ->
    %% Interrupt a running evaluation.
    %% Session is stripped from Params by the protocol decoder — use get_session(Msg).
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
            {status, ok};
        exit:{timeout, _} ->
            %% The shell did not answer in 5 s: it is alive but wedged. Say so
            %% rather than letting the exit crash the caller.
            {error,
                beamtalk_repl_errors:make(
                    session_busy,
                    'REPL',
                    <<"Session did not answer">>,
                    <<"The session is busy; try again.">>
                )}
    end.

%% BT-5199: reply only once the actor is gone, so a client that reads `actors`
%% right after the reply (the VS Code and LiveView "Kill actor" actions do)
%% never lists it. `exit/2` only sends the signal. Bounded: a process that is
%% somehow still alive after 5 s is left to die on its own.
-define(KILL_WAIT_MS, 5000).

-spec kill_and_wait(pid()) -> ok.
kill_and_wait(Pid) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _Reason} -> ok
    after ?KILL_WAIT_MS ->
        erlang:demonitor(MRef, [flush]),
        ok
    end.

%% Why `kill` refuses a pid the actor scan does not list: it has stopped, or it
%% is a live process that is not an actor (BT-5199).
-spec not_killable_error(pid(), binary()) -> beamtalk_error:error().
not_killable_error(Pid, PidBin) ->
    case is_process_alive(Pid) of
        true ->
            beamtalk_repl_errors:make(
                not_an_actor,
                'Actor',
                iolist_to_binary([<<"Not an actor: ">>, PidBin]),
                <<"Use :actors to find actors.">>
            );
        false ->
            beamtalk_repl_errors:make(
                actor_not_alive,
                'Actor',
                iolist_to_binary([<<"Actor is not alive: ">>, PidBin])
            )
    end.
