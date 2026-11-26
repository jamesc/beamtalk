%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_process).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for process navigation: `children` and `process-info` (BT-5205).

They are the one implementation of two reads the LiveView Inspector used to
make by calling `beamtalk_process_navigation:child_handles/1` and
`erlang:process_info/2` on the workspace node itself.

| Op | Chokepoint |
|----|------------|
| `children` | `beamtalk_process_navigation:child_handles/1` |
| `process-info` | `erlang:process_info/2` |

Both take a pid as the node-local text (`<0.123.0>`) or, from a client
attached over Erlang distribution, as the live pid, so the client never has to
format a remote pid. Neither needs the pid to be an actor: the first lists a
supervisor's children, the second reads the same few facts for any local
process (a future, a bare pid).

## `children`

Param `supervisor`. `{children, #{<<"children">> => [Row], <<"truncated">> => boolean()}}`, in `which_children`
order, at most 500 rows (`truncated` says whether it cut). A row is
`label`, `className`, `kind`, `pid`, `registeredName`, `childCount`,
`isSupervisor` and `handle`. `handle` is the live
`{beamtalk_supervisor | beamtalk_object, Class, Module, Pid}` term the
Inspector follows into the child, or `null` for a foreign or restarting child
(shown, not drillable). On the wire (`beamtalk_repl_ops:encode/2`) a handle is a stable id:
`#{<<"kind">>, <<"class">>, <<"pid">>}`, with `pid` the node-local text. A
dead supervisor is `{error, #beamtalk_error{}}` (`stale_handle`).

## `process-info`

Param `pid`. `{value, Map}` with `pid`, `alive`, and for a live process
`status`, `message_queue_len`, `memory` and `reductions`. A dead process is
`alive: false` and nothing else, never an error: the term is a handle that
was live a moment ago.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, handle_term/4, describe_ops/0, wire_children/1]).

-define(MAX_CHILDREN, 500).

-doc """
WebSocket-edge wrapper: encodes the term result to JSON. A `children` result's
live child `handles` become stable ids inside `beamtalk_repl_ops:encode/2`.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc "Term-returning handler for `children` and `process-info`.".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"children">>, Params, _Msg, _SessionPid) ->
    case beamtalk_repl_params:process_pid(Params, <<"supervisor">>) of
        {ok, _Pid, PidBin} ->
            case beamtalk_process_navigation:child_handles(PidBin) of
                {ok, Rows} ->
                    {Kept, Truncated} = cap(Rows),
                    {children, #{<<"children">> => Kept, <<"truncated">> => Truncated}};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end;
        {error, _} = Err ->
            Err
    end;
handle_term(<<"process-info">>, Params, _Msg, _SessionPid) ->
    case beamtalk_repl_params:process_pid(Params, <<"pid">>) of
        {ok, Pid, PidBin} ->
            {value, read_process_info(Pid, PidBin)};
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Results
%%====================================================================

-spec cap([map()]) -> {[map()], boolean()}.
cap(Rows) when length(Rows) > ?MAX_CHILDREN -> {lists:sublist(Rows, ?MAX_CHILDREN), true};
cap(Rows) -> {Rows, false}.

%% `process_info/2` answers `undefined` for a pid that has exited, and raises
%% `badarg` for a pid that is not on this node: both read as not alive.
-spec read_process_info(pid(), binary()) -> map().
read_process_info(Pid, PidBin) ->
    Keys = [status, message_queue_len, memory, reductions],
    Info =
        try
            erlang:process_info(Pid, Keys)
        catch
            error:badarg -> undefined
        end,
    case Info of
        undefined ->
            #{<<"pid">> => PidBin, <<"alive">> => false};
        Facts when is_list(Facts) ->
            #{
                <<"pid">> => PidBin,
                <<"alive">> => true,
                <<"status">> => atom_to_binary(proplists:get_value(status, Facts), utf8),
                <<"message_queue_len">> => proplists:get_value(message_queue_len, Facts),
                <<"memory">> => proplists:get_value(memory, Facts),
                <<"reductions">> => proplists:get_value(reductions, Facts)
            }
    end.

%% The wire form of a `children` result: each live handle becomes its stable
%% id, since JSON cannot carry a tuple holding a pid. Called by
%% `beamtalk_repl_ops:encode/2`.
-doc false.
-spec wire_children(map()) -> map().
wire_children(#{<<"children">> := Rows} = Result) ->
    Result#{<<"children">> := [wire_row(Row) || Row <- Rows]}.

-spec wire_row(map()) -> map().
wire_row(#{<<"handle">> := {Tag, Class, _Module, Pid}} = Row) when is_pid(Pid) ->
    Row#{
        <<"handle">> := #{
            <<"kind">> => handle_kind(Tag),
            <<"class">> => atom_to_binary(Class, utf8),
            <<"pid">> => list_to_binary(pid_to_list(Pid))
        }
    };
wire_row(Row) ->
    Row.

-spec handle_kind(beamtalk_supervisor | beamtalk_object) -> binary().
handle_kind(beamtalk_supervisor) -> <<"supervisor">>;
handle_kind(beamtalk_object) -> <<"actor">>.

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the process-navigation ops.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).
