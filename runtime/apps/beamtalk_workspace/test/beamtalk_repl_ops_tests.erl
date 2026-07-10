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
    {protocol_msg, Op, <<"id-1">>, undefined, #{}}.

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

dispatch_close_returns_status_term_test() ->
    %% BT-2402: `close` is now ported to a native term shape — the {json, Binary}
    %% escape tag was removed once every op returned a native op_result().
    Msg = make_msg(<<"close">>),
    Result = beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, self()),
    ?assertEqual({status, ok}, Result).

dispatch_sessions_no_supervisor_returns_sessions_term_test() ->
    %% No beamtalk_session_sup in unit tests → empty sessions list term.
    Msg = make_msg(<<"sessions">>),
    ?assertEqual({sessions, []}, beamtalk_repl_ops:dispatch(<<"sessions">>, #{}, Msg, self())).

dispatch_describe_returns_describe_term_test() ->
    Msg = make_msg(<<"describe">>),
    Result = beamtalk_repl_ops:dispatch(<<"describe">>, #{}, Msg, self()),
    ?assertMatch({describe, Ops, Versions} when is_map(Ops) andalso is_map(Versions), Result).

dispatch_load_source_empty_returns_error_term_test() ->
    Msg = make_msg(<<"load-source">>),
    Result = beamtalk_repl_ops:dispatch(<<"load-source">>, #{<<"source">> => <<>>}, Msg, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_nav_query_missing_kind_returns_error_term_test() ->
    Msg = make_msg(<<"nav-query">>),
    Result = beamtalk_repl_ops:dispatch(<<"nav-query">>, #{}, Msg, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_complete_returns_completions_term_test() ->
    %% Empty prefix → empty completions list, no runtime registry needed.
    Msg = make_msg(<<"complete">>),
    Result = beamtalk_repl_ops:dispatch(<<"complete">>, #{<<"code">> => <<>>}, Msg, self()),
    ?assertEqual({completions, []}, Result).

dispatch_methods_unknown_class_returns_methods_term_test() ->
    Msg = make_msg(<<"methods">>),
    Result = beamtalk_repl_ops:dispatch(
        <<"methods">>, #{<<"class">> => <<"NoSuchClassXyz">>}, Msg, self()
    ),
    ?assertEqual({methods, [], []}, Result).

dispatch_unload_unknown_class_returns_error_term_test() ->
    Msg = make_msg(<<"unload">>),
    Result = beamtalk_repl_ops:dispatch(
        <<"unload">>, #{<<"module">> => <<"NoSuchClassXyz">>}, Msg, self()
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_show_codegen_no_params_returns_error_term_test() ->
    Msg = make_msg(<<"show-codegen">>),
    Result = beamtalk_repl_ops:dispatch(<<"show-codegen">>, #{}, Msg, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result).

dispatch_tracing_invalid_arg_returns_error_term_test() ->
    %% BT-2402: the tracing ops normalise invalid filter args into an {error, _}
    %% term so dist clients always get an op_result(). An invalid actor PID is
    %% rejected during filter parsing before the trace store is touched, so this
    %% runs without a running store.
    Msg = make_msg(<<"get-traces">>),
    Result = beamtalk_repl_ops:dispatch(
        <<"get-traces">>, #{<<"actor">> => <<"not-a-pid">>}, Msg, self()
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result).

%% BT-2402: round-trip — encoding a dispatched term reproduces the handle_op JSON.
encode_completions_term_matches_handle_op_json_test() ->
    Msg = make_msg(<<"complete">>),
    Params = #{<<"code">> => <<>>},
    Term = beamtalk_repl_ops:dispatch(<<"complete">>, Params, Msg, self()),
    Encoded = beamtalk_repl_ops:encode(Term, Msg),
    ViaServer = beamtalk_repl_server:handle_op(<<"complete">>, Params, Msg, self()),
    ?assertEqual(ViaServer, Encoded),
    Decoded = json:decode(Encoded),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)).

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

encode_value_term_is_identity_encoded_test() ->
    %% BT-2402: the `value` tag carries an already-wire-shaped JSON value, encoded
    %% with identity (no term_to_json). A map of binaries/lists survives intact.
    Msg = make_msg(<<"nav-query">>),
    Value = #{<<"sites">> => []},
    Encoded = beamtalk_repl_ops:encode({value, Value}, Msg),
    Decoded = json:decode(Encoded),
    ?assertEqual([], maps:get(<<"sites">>, maps:get(<<"value">>, Decoded))),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_describe_term_matches_handle_op_json_test() ->
    %% Encoding a dispatched term at the edge must reproduce exactly what the
    %% WebSocket transport returns via handle_op/4 (wire format unchanged).
    Msg = make_msg(<<"describe">>),
    Term = beamtalk_repl_ops:dispatch(<<"describe">>, #{}, Msg, self()),
    Encoded = beamtalk_repl_ops:encode(Term, Msg),
    ViaServer = beamtalk_repl_server:handle_op(<<"describe">>, #{}, Msg, self()),
    ?assertEqual(ViaServer, Encoded).

%%====================================================================
%% beamtalk_repl_subscriptions — facade
%%====================================================================

streams_lists_all_six_push_streams_test() ->
    ?assertEqual(
        [transcript, actors, classes, bindings, flush, reload_check],
        beamtalk_repl_subscriptions:streams()
    ).

subscribe_all_returns_ok_test() ->
    %% BT-2531: the bus-backed streams register on the SystemAnnouncer bus, so it
    %% must be running; the transcript stream is a tolerant cast. The facade
    %% returns ok across every stream.
    ensure_announcements_bus(),
    ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe_all()),
    ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe_all()).

subscribe_single_stream_returns_ok_test() ->
    ensure_announcements_bus(),
    [
        ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe(S))
     || S <- beamtalk_repl_subscriptions:streams()
    ],
    [
        ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe(S))
     || S <- beamtalk_repl_subscriptions:streams()
    ].

%% Start the announcements bus if not already running. Start the full runtime
%% app (which supervises the bus) rather than a bare gen_server: a standalone
%% registered bus would leak past this module and break later test modules'
%% application:ensure_all_started(beamtalk_runtime) with already_started.
ensure_announcements_bus() ->
    case whereis(beamtalk_announcements) of
        undefined ->
            {ok, _} = application:ensure_all_started(beamtalk_runtime),
            ok;
        _ ->
            ok
    end.
