%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_eval_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for `beamtalk_repl_ops_eval` — the `eval` op handler (BT-2399,
ADR 0017 Phase 3).

Covers:
* empty code → `{error, #beamtalk_error{kind=empty_expression}}` (no session
  needed — short-circuits before touching the session pid);
* non-empty expression that succeeds → `{ok, Value, Output, Warnings}`;
* non-empty expression that fails at runtime → `{error, #beamtalk_error{}, Output, Warnings}`
  (verify `beamtalk_repl_errors:ensure_structured_error/1` wrapping);
* trace mode → `{trace, Steps, Output, Warnings}`;
* `handle/4` WebSocket edge wrapper → encodes the term result to a JSON binary.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op) ->
    {protocol_msg, Op, <<"id-1">>, undefined, #{}}.

setup(SessionId) ->
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    %% Allow the runtime to register its bootstrap classes before compiling.
    timer:sleep(300),
    {ok, SessionPid} = beamtalk_repl_shell:start_link(SessionId),
    SessionPid.

teardown(SessionPid) ->
    catch beamtalk_repl_shell:stop(SessionPid),
    %% Restore baseline "compiler not running" state so subsequent test modules
    %% that test no-compiler error paths see the expected initial state.
    _ = application:stop(beamtalk_compiler),
    ok.

%%====================================================================
%% handle_term/4 — empty code (no session required)
%%====================================================================

handle_term_empty_code_returns_empty_expression_error_test() ->
    Msg = make_msg(<<"eval">>),
    Result = beamtalk_repl_ops_eval:handle_term(
        <<"eval">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    ?assertMatch({error, #beamtalk_error{kind = empty_expression}}, Result).

%%====================================================================
%% handle_term/4 — non-empty eval paths (session required)
%%====================================================================

handle_term_non_empty_eval_success_test_() ->
    {setup, fun() -> setup(<<"test-eval-ops-success">>) end, fun teardown/1, fun(SessionPid) ->
        [
            ?_test(begin
                Msg = make_msg(<<"eval">>),
                Result = beamtalk_repl_ops_eval:handle_term(
                    <<"eval">>, #{<<"code">> => <<"1 + 2">>}, Msg, SessionPid
                ),
                ?assertMatch({ok, 3, _, _}, Result)
            end)
        ]
    end}.

handle_term_eval_error_wraps_in_beamtalk_error_test_() ->
    {setup, fun() -> setup(<<"test-eval-ops-error">>) end, fun teardown/1, fun(SessionPid) ->
        [
            ?_test(begin
                Msg = make_msg(<<"eval">>),
                %% Unary message not understood by Integer → does_not_understand.
                Result = beamtalk_repl_ops_eval:handle_term(
                    <<"eval">>, #{<<"code">> => <<"1 unknownMsg">>}, Msg, SessionPid
                ),
                ?assertMatch({error, #beamtalk_error{}, _, _}, Result)
            end)
        ]
    end}.

%%====================================================================
%% handle_term/4 — trace mode
%%====================================================================

handle_term_trace_mode_returns_trace_shape_test_() ->
    {setup, fun() -> setup(<<"test-eval-ops-trace">>) end, fun teardown/1, fun(SessionPid) ->
        [
            ?_test(begin
                Msg = make_msg(<<"eval">>),
                Result = beamtalk_repl_ops_eval:handle_term(
                    <<"eval">>,
                    #{<<"code">> => <<"1 + 2">>, <<"trace">> => true},
                    Msg,
                    SessionPid
                ),
                ?assertMatch({trace, _, _, _}, Result)
            end)
        ]
    end}.

%%====================================================================
%% handle/4 — WebSocket edge wrapper
%%====================================================================

handle_websocket_encodes_eval_result_to_json_test_() ->
    {setup, fun() -> setup(<<"test-eval-ops-ws">>) end, fun teardown/1, fun(SessionPid) ->
        [
            ?_test(begin
                Msg = make_msg(<<"eval">>),
                Result = beamtalk_repl_ops_eval:handle(
                    <<"eval">>, #{<<"code">> => <<"1 + 2">>}, Msg, SessionPid
                ),
                ?assert(is_binary(Result)),
                Decoded = json:decode(Result),
                ?assert(is_map(Decoded)),
                ?assert(maps:is_key(<<"value">>, Decoded)),
                ?assertEqual(3, maps:get(<<"value">>, Decoded))
            end)
        ]
    end}.
