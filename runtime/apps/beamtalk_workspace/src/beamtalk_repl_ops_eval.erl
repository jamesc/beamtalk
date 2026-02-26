%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for eval, clear, and bindings operations.
%%%
%%% **DDD Context:** REPL
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_eval).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4]).

%% @doc Handle eval/clear/bindings ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"eval">>, Params, Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to evaluate.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
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
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
    %% ADR 0019 Phase 3: Filter out workspace convenience bindings from display.
    UserBindings = maps:without(beamtalk_workspace_config:binding_names(), Bindings),
    beamtalk_repl_protocol:encode_bindings(
        UserBindings, Msg, fun beamtalk_repl_json:term_to_json/1
    ).
