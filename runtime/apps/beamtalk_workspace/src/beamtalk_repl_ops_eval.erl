%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_eval).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handler for the `eval` operation.

Extracted from beamtalk_repl_server (BT-705). The `clear` and `bindings`
ops were removed in BT-2369 (ADR 0081 Phase 6) — session state is now read
and mutated through the Beamtalk-native `Session` API
(`Session current bindings`, `Session current clear`) via `eval`.

`eval` is the canonical term-returning core path (BT-2399, ADR 0017 Phase 3):
`handle_term/4` returns a structured `beamtalk_repl_ops:op_result()` term and
never produces JSON. `handle/4` is the WebSocket-edge wrapper that encodes that
term to the protocol JSON binary via `beamtalk_repl_ops:encode/2`.
""".

-export([handle/4, handle_term/4]).

-doc """
Handle the eval op for the WebSocket transport — encodes the term result to
JSON at the edge.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"eval">>, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(<<"eval">>, Params, Msg, SessionPid), Msg).

-doc """
Term-returning eval handler. Returns `{ok, Value, Output, Warnings}` on success,
`{trace, Steps, Output, Warnings}` in trace mode,
`{script_exit, Code, Output, Warnings}` when the expression called
`Program exit: Code` in this connected session (BT-2688), or
`{error, #beamtalk_error{}}` / `{error, #beamtalk_error{}, Output, Warnings}` on
failure. No JSON in this path — dist-attached clients consume the term directly.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"eval">>, Params, _Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    Trace = maps:get(<<"trace">>, Params, false) =:= true,
    case Code of
        [] ->
            {error,
                beamtalk_repl_errors:make(
                    empty_expression,
                    'REPL',
                    <<"Empty expression">>,
                    <<"Enter an expression to evaluate.">>
                )};
        _ when Trace ->
            case beamtalk_repl_shell:eval_trace(SessionPid, Code) of
                {ok, Steps, Output, Warnings} ->
                    {trace, Steps, Output, Warnings};
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    {error, WrappedReason, Output, Warnings}
            end;
        _ ->
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    {ok, Result, Output, Warnings};
                {script_exit, Code2, Output, Warnings} ->
                    %% BT-2688: `Program exit: Code2` ended this connected session.
                    {script_exit, Code2, Output, Warnings};
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    {error, WrappedReason, Output, Warnings}
            end
    end.
