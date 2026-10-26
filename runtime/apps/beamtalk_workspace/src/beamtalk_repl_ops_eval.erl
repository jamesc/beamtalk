%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_eval).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handler for the `eval` operation.

Extracted from beamtalk_repl_server. The `clear` and `bindings`
ops have been removed (ADR 0081 Phase 6) — session state is now read
and mutated through the Beamtalk-native `Session` API
(`Session current bindings`, `Session current clear`) via `eval`.

`eval` is the canonical term-returning core path (ADR 0017 Phase 3):
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
`Program exit: Code` in this connected session, or
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
                {script_exit, Code2, Output, Warnings} ->
                    %% `Program exit: Code2` inside a traced eval — same
                    %% connected-exit handling as the non-trace branch.
                    {script_exit, Code2, Output, Warnings};
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    {error, WrappedReason, Output, Warnings}
            end;
        _ ->
            CodeBin = maps:get(<<"code">>, Params, <<>>),
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    {ok, maybe_tag_character(Result, CodeBin), Output, Warnings};
                {script_exit, Code2, Output, Warnings} ->
                    %% `Program exit: Code2` ended this connected session.
                    {script_exit, Code2, Output, Warnings};
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    {error, WrappedReason, Output, Warnings}
            end
    end.

-doc """
Tag a Character-typed integer result for distinct REPL display (BT-4167).

When `Result` is an integer in the Unicode scalar range, queries the compiler's
static type inference (`resolve_completion_type`, same API as the completion
fallback in `beamtalk_repl_ops_dev`) for `CodeBin`'s last expression. A
`Character` answer tags the value as `{beamtalk_character, CP}` so
`beamtalk_repl_json:term_to_json/1` renders `$X` instead of the codepoint
decimal — distinct from Integers. Any other type, unknown type, missing
compiler (release node), or query failure falls back to the raw integer
(today's display). Non-integers and out-of-range integers skip the query
entirely.
""".
-spec maybe_tag_character(term(), binary()) -> term().
maybe_tag_character(Result, CodeBin) when is_integer(Result), is_binary(CodeBin) ->
    case
        Result >= 0 andalso Result =< 16#10FFFF andalso
            not (Result >= 16#D800 andalso Result =< 16#DFFF)
    of
        false ->
            Result;
        true ->
            case beamtalk_capability:available(completion_type_inference) of
                false ->
                    Result;
                true ->
                    try beamtalk_compiler:resolve_completion_type(CodeBin) of
                        {ok, 'Character'} -> {beamtalk_character, Result};
                        _ -> Result
                    catch
                        _:_ -> Result
                    end
            end
    end;
maybe_tag_character(Result, _CodeBin) ->
    Result.
