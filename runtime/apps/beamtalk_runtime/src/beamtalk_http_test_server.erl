%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Simple cowboy-based HTTP test server for BUnit integration tests (BT-1117).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Starts a minimal HTTP/1.1 server on an ephemeral port and exposes test
%%% endpoints used by the BUnit `http_test.bt` suite.  The server uses the
%%% cowboy listener name `beamtalk_http_bunit_test` so callers can easily
%%% stop it between tests.
%%%
%%% ## Endpoints
%%%
%%% | Path              | Method | Status | Response body                              |
%%% |-------------------|--------|--------|--------------------------------------------|
%%% | `/get`            | GET    | 200    | `{"method":"GET"}`                         |
%%% | `/post`           | POST   | 200    | `{"method":"POST","body":"<req-body>"}`    |
%%% | `/put`            | PUT    | 200    | `{"method":"PUT","body":"<req-body>"}`     |
%%% | `/delete`         | DELETE | 200    | `{"method":"DELETE"}`                      |
%%% | `/status/:code`   | any    | :code  | empty                                      |
%%% | `/json`           | GET    | 200    | `{"type":"json","value":42}`               |
%%% | `/echo-headers`   | any    | 200    | JSON map of received request headers       |
%%%
%%% ## Usage (from Beamtalk)
%%%
%%% ```beamtalk
%%% port := (Erlang beamtalk_http_test_server) start
%%% // ... run tests ...
%%% (Erlang beamtalk_http_test_server) stop
%%% ```

-module(beamtalk_http_test_server).

-export([start/0, stop/0, get_port/0]).

-define(LISTENER, beamtalk_http_bunit_test).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start the test HTTP server on an ephemeral port.
%%
%% Ensures cowboy and gun are running (they may not be in the BUnit test
%% context, which does not start `beamtalk_workspace`).
%%
%% Returns the TCP port the server is listening on.
-spec start() -> non_neg_integer().
start() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/get", beamtalk_test_handler, get},
            {"/post", beamtalk_test_handler, post},
            {"/put", beamtalk_test_handler, put},
            {"/delete", beamtalk_test_handler, delete},
            {"/status/:code", beamtalk_test_handler, status},
            {"/json", beamtalk_test_handler, json},
            {"/echo-headers", beamtalk_test_handler, echo_headers}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}], #{env => #{dispatch => Dispatch}}),
    ranch:get_port(?LISTENER).

%% @doc Stop the test HTTP server.
%%
%% Safe to call even when the server is not running.
-spec stop() -> ok.
stop() ->
    _ = catch cowboy:stop_listener(?LISTENER),
    ok.

%% @doc Return the port the test server is currently listening on.
%%
%% Must only be called after `start/0`.
-spec get_port() -> non_neg_integer().
get_port() ->
    ranch:get_port(?LISTENER).
