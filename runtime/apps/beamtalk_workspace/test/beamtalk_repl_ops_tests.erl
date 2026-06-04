%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for the term-returning op seam (`beamtalk_repl_ops`) and the live
push-stream subscription facade (`beamtalk_repl_subscriptions`), BT-2399.

These assert the *term* contract (`op_result()`) that dist-attached clients
consume — distinct from the JSON wire format covered by the per-op handle/4
tests. They also assert that encoding a dispatched term at the edge reproduces
the same JSON the WebSocket transport returns via `handle_op/4`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op) ->
    make_msg(Op, false).

make_msg(Op, Legacy) ->
    {protocol_msg, Op, <<"id-1">>, undefined, #{}, Legacy}.

%%====================================================================
%% dispatch/4 — term shapes (no JSON in the core path)
%%====================================================================

dispatch_eval_empty_code_returns_error_term_test() ->
    Msg = make_msg(<<"eval">>),
    %% Empty code short-circuits before touching the session pid.
    Result = beamtalk_repl_ops:dispatch(<<"eval">>, #{<<"code">> => <<>>}, Msg, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_actors_no_registry_returns_actors_term_test() ->
    Msg = make_msg(<<"actors">>),
    %% No beamtalk_actor_registry in unit tests → empty actor list term.
    ?assertEqual({actors, []}, beamtalk_repl_ops:dispatch(<<"actors">>, #{}, Msg, self())).

dispatch_inspect_invalid_pid_returns_error_term_test() ->
    Msg = make_msg(<<"inspect">>),
    Result = beamtalk_repl_ops:dispatch(
        <<"inspect">>, #{<<"actor">> => <<"not-a-pid">>}, Msg, self()
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_kill_invalid_pid_returns_error_term_test() ->
    Msg = make_msg(<<"kill">>),
    Result = beamtalk_repl_ops:dispatch(
        <<"kill">>, #{<<"actor">> => <<"bad-pid">>}, Msg, self()
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_interrupt_dead_session_returns_status_term_test() ->
    %% Dead SessionPid → noproc caught → {status, ok} term.
    Pid = spawn(fun() -> ok end),
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end,
    Msg = make_msg(<<"interrupt">>),
    ?assertEqual({status, ok}, beamtalk_repl_ops:dispatch(<<"interrupt">>, #{}, Msg, Pid)).

dispatch_unknown_op_returns_error_term_test() ->
    Msg = make_msg(<<"no-such-op">>),
    Result = beamtalk_repl_ops:dispatch(<<"no-such-op">>, #{}, Msg, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result),
    {error, Err} = Result,
    ?assertEqual(unknown_op, Err#beamtalk_error.kind).

dispatch_unported_op_returns_json_passthrough_test() ->
    %% `close` is not yet ported to a native term shape — it is surfaced via the
    %% {json, Binary} escape tag so it still flows through the one seam.
    Msg = make_msg(<<"close">>),
    Result = beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, self()),
    ?assertMatch({json, Bin} when is_binary(Bin), Result).

%%====================================================================
%% encode/2 — term result → JSON at the WebSocket edge
%%====================================================================

encode_actors_term_matches_handle_op_json_test() ->
    %% Encoding a dispatched term at the edge must reproduce exactly what the
    %% WebSocket transport returns via handle_op/4 (wire format unchanged).
    Msg = make_msg(<<"actors">>),
    Term = beamtalk_repl_ops:dispatch(<<"actors">>, #{}, Msg, self()),
    Encoded = beamtalk_repl_ops:encode(Term, Msg),
    ViaServer = beamtalk_repl_server:handle_op(<<"actors">>, #{}, Msg, self()),
    ?assertEqual(ViaServer, Encoded),
    Decoded = json:decode(Encoded),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)).

encode_error_term_produces_error_json_test() ->
    Msg = make_msg(<<"inspect">>),
    Term = beamtalk_repl_ops:dispatch(
        <<"inspect">>, #{<<"actor">> => <<"not-a-pid">>}, Msg, self()
    ),
    Decoded = json:decode(beamtalk_repl_ops:encode(Term, Msg)),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(binary:match(maps:get(<<"error">>, Decoded), <<"Invalid actor PID">>) =/= nomatch).

encode_json_passthrough_is_identity_test() ->
    Msg = make_msg(<<"close">>),
    Bin = beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1),
    ?assertEqual(Bin, beamtalk_repl_ops:encode({json, Bin}, Msg)).

%%====================================================================
%% beamtalk_repl_subscriptions — facade
%%====================================================================

streams_lists_all_five_push_streams_test() ->
    ?assertEqual(
        [transcript, actors, classes, bindings, flush],
        beamtalk_repl_subscriptions:streams()
    ).

subscribe_all_returns_ok_test() ->
    %% The underlying event servers are not running in unit tests; gen_server:cast
    %% to an unregistered name is a silent no-op, so the facade still returns ok.
    ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe_all()),
    ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe_all()).

subscribe_single_stream_returns_ok_test() ->
    [
        ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe(S))
     || S <- beamtalk_repl_subscriptions:streams()
    ],
    [
        ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe(S))
     || S <- beamtalk_repl_subscriptions:streams()
    ].
