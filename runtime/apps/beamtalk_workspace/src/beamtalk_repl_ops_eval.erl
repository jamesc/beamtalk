%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_eval).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for eval, clear, and bindings operations.

Extracted from beamtalk_repl_server (BT-705).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4]).

-doc "Handle eval/clear/bindings ops.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"eval">>, Params, Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    Trace = maps:get(<<"trace">>, Params, false) =:= true,
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to evaluate.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ when Trace ->
            case beamtalk_repl_shell:eval_trace(SessionPid, Code) of
                {ok, Steps, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_trace_result(
                        Steps, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
                    );
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1,
                        Output,
                        Warnings
                    )
            end;
        _ ->
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_result(
                        Result, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
                    );
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1,
                        Output,
                        Warnings
                    )
            end
    end;
handle(<<"clear">>, _Params, Msg, SessionPid) ->
    ok = beamtalk_repl_shell:clear_bindings(SessionPid),
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
handle(<<"bindings">>, _Params, Msg, SessionPid) ->
    %% BT-1045: session is stripped from Params by the protocol decoder — use get_session(Msg).
    %% This allows the VS Code extension (which has its own empty WS session) to request
    %% bindings for the user's REPL terminal session instead.
    TargetPid = beamtalk_session_table:resolve_pid(
        beamtalk_repl_protocol:get_session(Msg), SessionPid
    ),
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(TargetPid),
    %% ADR 0019 Phase 3: Filter out workspace convenience bindings from display.
    UserBindings = maps:without(beamtalk_workspace_config:binding_names(), Bindings),
    beamtalk_repl_protocol:encode_bindings(
        UserBindings, Msg, fun beamtalk_repl_json:term_to_json/1
    ).
