%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http_test_server (BT-1117).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Verifies lifecycle operations on the cowboy-backed HTTP test server.
%%% These tests start a real listener and make a real HTTP request to confirm
%%% the server is functioning before the BUnit suite runs against it.

-module(beamtalk_http_test_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Lifecycle tests
%%% ============================================================================

start_returns_valid_port_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Port = beamtalk_http_test_server:start(),
    ?assert(is_integer(Port)),
    ?assert(Port > 0),
    ?assert(Port =< 65535),
    beamtalk_http_test_server:stop().

get_port_matches_start_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Port = beamtalk_http_test_server:start(),
    ?assertEqual(Port, beamtalk_http_test_server:get_port()),
    beamtalk_http_test_server:stop().

stop_is_idempotent_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    _Port = beamtalk_http_test_server:start(),
    ?assertEqual(ok, beamtalk_http_test_server:stop()),
    %% Second stop should not crash
    ?assertEqual(ok, beamtalk_http_test_server:stop()).

start_stop_start_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Port1 = beamtalk_http_test_server:start(),
    beamtalk_http_test_server:stop(),
    Port2 = beamtalk_http_test_server:start(),
    ?assert(is_integer(Port2)),
    ?assert(Port2 > 0),
    %% Ports may differ between restarts (ephemeral port assignment)
    _ = Port1,
    beamtalk_http_test_server:stop().

%%% ============================================================================
%%% Integration smoke test (requires gun)
%%% ============================================================================

get_endpoint_returns_200_test() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    Port = beamtalk_http_test_server:start(),
    Url = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/get"]),
    Resp = beamtalk_http:'get:'(Url),
    ?assertEqual(200, maps:get(status, Resp)),
    beamtalk_http_test_server:stop().
