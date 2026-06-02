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
""".

-export([handle/4]).

-doc "Handle the eval op.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"eval">>, Params, Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    Trace = maps:get(<<"trace">>, Params, false) =:= true,
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to evaluate.">>),
            beamtalk_repl_json:encode_error(Err2, Msg);
        _ when Trace ->
            case beamtalk_repl_shell:eval_trace(SessionPid, Code) of
                {ok, Steps, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_trace_result(
                        Steps, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
                    );
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    beamtalk_repl_json:encode_error(WrappedReason, Msg, Output, Warnings)
            end;
        _ ->
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_result(
                        Result, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
                    );
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    beamtalk_repl_json:encode_error(WrappedReason, Msg, Output, Warnings)
            end
    end.
