%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http_server (BT-1338).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Verifies the cowboy lifecycle functions called by the HTTPServer actor.
%%% Tests start real listeners and make real HTTP requests to confirm
%%% the server is functioning correctly.

-module(beamtalk_http_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Lifecycle tests (direct Erlang API)
%%% ============================================================================

start_listener_returns_ref_and_port_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    [Ref, Port] = beamtalk_http_server:startListener(0, Handler),
    ?assert(is_reference(Ref)),
    ?assert(is_integer(Port)),
    ?assert(Port > 0),
    ?assert(Port =< 65535),
    beamtalk_http_server:stopListener(Ref).

stop_listener_is_idempotent_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    [Ref, _Port] = beamtalk_http_server:startListener(0, Handler),
    ?assertEqual(ok, beamtalk_http_server:stopListener(Ref)),
    ?assertEqual(ok, beamtalk_http_server:stopListener(Ref)).

start_multiple_listeners_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    [Ref1, Port1] = beamtalk_http_server:startListener(0, Handler),
    [Ref2, Port2] = beamtalk_http_server:startListener(0, Handler),
    ?assertNotEqual(Port1, Port2),
    beamtalk_http_server:stopListener(Ref1),
    beamtalk_http_server:stopListener(Ref2).

invalid_port_raises_type_error_test() ->
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:startListener(-1, Handler)
    ).

invalid_handler_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:startListener(0, not_a_handler)
    ).

invalid_opts_raises_type_error_test() ->
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:startListener(0, Handler, not_a_map)
    ).

invalid_bind_raises_type_error_test() ->
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:startListener(0, Handler, #{bind => 123})
    ).

%%% ============================================================================
%%% Integration smoke test (requires gun)
%%% ============================================================================

handler_receives_request_and_returns_response_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    Handler = fun(_Req) -> make_simple_response(200, <<"hello from handler">>) end,
    [Ref, Port] = beamtalk_http_server:startListener(0, Handler),
    Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/test"]),
    #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Resp} =
        beamtalk_http:'get:'(Url),
    ?assertEqual(200, maps:get(status, Resp)),
    ?assertEqual(<<"hello from handler">>, maps:get(body, Resp)),
    beamtalk_http_server:stopListener(Ref).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @private Build a minimal HTTPResponse map for testing.
%%
%% This constructs a raw map matching the HTTPResponse field layout,
%% bypassing the bt@stdlib@httpresponse constructor (which may not be
%% loaded during EUnit tests).
-spec make_simple_response(non_neg_integer(), binary()) -> map().
make_simple_response(Status, Body) ->
    #{
        '$beamtalk_class' => 'HTTPResponse',
        status => Status,
        headers => [],
        body => Body
    }.
