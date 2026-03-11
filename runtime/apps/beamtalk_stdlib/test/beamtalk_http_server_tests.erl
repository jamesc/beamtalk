%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http_server (BT-1338).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Verifies lifecycle operations on the cowboy-backed HTTP server wrapper.
%%% Tests start real listeners and make real HTTP requests to confirm
%%% the server is functioning correctly.

-module(beamtalk_http_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Lifecycle tests
%%% ============================================================================

start_returns_server_with_port_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    Result = beamtalk_http_server:start(0, Handler),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, Result),
    Server = maps:get('okValue', Result),
    ?assert(is_map(Server)),
    Port = maps:get(actualPort, Server),
    ?assert(is_integer(Port)),
    ?assert(Port > 0),
    ?assert(Port =< 65535),
    Ref = maps:get(listenerRef, Server),
    beamtalk_http_server:stop(Ref).

stop_is_idempotent_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    Server = unwrap_result(beamtalk_http_server:start(0, Handler)),
    Ref = maps:get(listenerRef, Server),
    ?assertEqual(ok, beamtalk_http_server:stop(Ref)),
    ?assertEqual(ok, beamtalk_http_server:stop(Ref)).

start_multiple_servers_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    Server1 = unwrap_result(beamtalk_http_server:start(0, Handler)),
    Server2 = unwrap_result(beamtalk_http_server:start(0, Handler)),
    Port1 = maps:get(actualPort, Server1),
    Port2 = maps:get(actualPort, Server2),
    ?assertNotEqual(Port1, Port2),
    beamtalk_http_server:stop(maps:get(listenerRef, Server1)),
    beamtalk_http_server:stop(maps:get(listenerRef, Server2)).

invalid_port_raises_type_error_test() ->
    Handler = fun(_Req) -> make_simple_response(200, <<"ok">>) end,
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:start(-1, Handler)
    ).

invalid_handler_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_http_server:start(0, not_a_handler)
    ).

%%% ============================================================================
%%% Integration smoke test (requires gun)
%%% ============================================================================

handler_receives_request_and_returns_response_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    Handler = fun(_Req) -> make_simple_response(200, <<"hello from handler">>) end,
    Server = unwrap_result(beamtalk_http_server:start(0, Handler)),
    Port = maps:get(actualPort, Server),
    Url = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/test"]),
    #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Resp} =
        beamtalk_http:'get:'(Url),
    ?assertEqual(200, maps:get(status, Resp)),
    ?assertEqual(<<"hello from handler">>, maps:get(body, Resp)),
    beamtalk_http_server:stop(maps:get(listenerRef, Server)).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @private Extract the ok value from a Result map.
-spec unwrap_result(map()) -> term().
unwrap_result(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Value}) ->
    Value.

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
