%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_watch).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handler for the live-Inspector pid-stats read (ADR 0095 §5, BT-2489 — Cockpit
Phase 3).

`pid-stats` is the read-op companion to the per-object change subscriptions
(`beamtalk_object_watch`): a Cockpit Inspector pane re-issues it on each
`{object_changed, ...}` push (or on a refresh timer) to display live process
metrics for the inspected actor — message queue depth, memory, reductions,
scheduling status, and current function. The same `process_info` primitive set
`:observer`/LiveDashboard use (ADR 0092 prior art), guarded so a dead or
unreachable pid degrades to a `status: dead` map rather than raising.

Per-object change *subscriptions* are not ops — they are live push streams routed
through `beamtalk_repl_subscriptions` (the `object` stream), the same facade the
transcript/bindings streams use. This module owns only the request/response
`pid-stats` read.

`handle_term/4` returns a native `beamtalk_repl_ops:op_result()` (`{value, Map}`
on success; `{error, #beamtalk_error{}}` on a bad pid), so dist-attached clients
receive the live metrics map; `handle/4` is the WebSocket-edge JSON wrapper.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

-doc "WebSocket-edge wrapper: encodes the term result to JSON (BT-2402).".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for `pid-stats`. Returns `{value, StatsMap}` with
binary-keyed live process metrics, or `{error, #beamtalk_error{}}` for an
invalid/unknown pid.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"pid-stats">>, Params, _Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, maps:get(<<"pid">>, Params, <<>>))),
    case beamtalk_repl_ops_actors:validate_actor_pid(PidStr) of
        {ok, Pid} ->
            {value, pid_stats(Pid)};
        {error, Reason} ->
            {error, invalid_pid_error(Reason, PidStr)}
    end.

%%% Internal helpers

-doc """
Collect live process metrics for `Pid` via `erlang:process_info/2`. Best-effort
and never raises: a dead or info-less pid yields a `status => dead` map. Keys are
binaries so the result encodes verbatim through the `{value, _}` identity path.
""".
-spec pid_stats(pid()) -> map().
pid_stats(Pid) ->
    PidBin = list_to_binary(pid_to_list(Pid)),
    case erlang:is_process_alive(Pid) of
        true ->
            case
                erlang:process_info(Pid, [
                    message_queue_len, memory, reductions, status, current_function
                ])
            of
                undefined ->
                    dead_stats(PidBin);
                Info ->
                    #{
                        <<"pid">> => PidBin,
                        <<"alive">> => true,
                        <<"status">> => atom_to_binary(
                            proplists:get_value(status, Info, undefined), utf8
                        ),
                        <<"queue_depth">> => proplists:get_value(message_queue_len, Info, 0),
                        <<"memory_bytes">> => proplists:get_value(memory, Info, 0),
                        <<"memory_kb">> => proplists:get_value(memory, Info, 0) div 1024,
                        <<"reductions">> => proplists:get_value(reductions, Info, 0),
                        <<"current_function">> => format_mfa(
                            proplists:get_value(current_function, Info, undefined)
                        )
                    }
            end;
        false ->
            dead_stats(PidBin)
    end.

-spec dead_stats(binary()) -> map().
dead_stats(PidBin) ->
    #{
        <<"pid">> => PidBin,
        <<"alive">> => false,
        <<"status">> => <<"dead">>
    }.

%% Render an {M, F, A} current-function tuple to a readable "m:f/a" binary; nil
%% when the VM reports no current function.
-spec format_mfa(undefined | {atom(), atom(), arity()}) -> binary() | nil.
format_mfa(undefined) ->
    nil;
format_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    iolist_to_binary(io_lib:format("~ts:~ts/~B", [M, F, A]));
format_mfa(_) ->
    nil.

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

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the live-inspector pid-stats op.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"pid-stats">> => #{
            <<"params">> => [<<"actor">>],
            <<"notes">> =>
                <<"Live process metrics (queue depth, memory, reductions, status) for an actor pid.">>
        }
    }.
