%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Cowboy request handler for BUnit HTTP integration tests (BT-1117).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Implements test endpoints used by `beamtalk_http_test_server`.  Each
%%% endpoint is identified by the `State` argument passed from the router:
%%%
%%% | State          | Path             | Behaviour                                |
%%% |----------------|------------------|------------------------------------------|
%%% | `get`          | `/get`           | 200, JSON `{"method":"GET"}`             |
%%% | `post`         | `/post`          | 200, JSON `{"method":"POST","body":"…"}` |
%%% | `put`          | `/put`           | 200, JSON `{"method":"PUT","body":"…"}`  |
%%% | `delete`       | `/delete`        | 200, JSON `{"method":"DELETE"}`          |
%%% | `status`       | `/status/:code`  | `:code` status, empty body               |
%%% | `json`         | `/json`          | 200, `application/json`                  |
%%% | `echo_headers` | `/echo-headers`  | 200, JSON map of request headers         |

-module(beamtalk_test_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-define(JSON_CT, #{<<"content-type">> => <<"application/json">>}).

%% @doc Handle a cowboy HTTP request.
%%
%% The `State` atom identifies which test endpoint to serve.
-spec init(cowboy_req:req(), atom()) -> {ok, cowboy_req:req(), atom()}.
init(Req0, get) ->
    Body = jsx:encode(#{<<"method">> => <<"GET">>}),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req0),
    {ok, Req, get};
init(Req0, post) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    Body = jsx:encode(#{<<"method">> => <<"POST">>, <<"body">> => ReqBody}),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req1),
    {ok, Req, post};
init(Req0, put) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    Body = jsx:encode(#{<<"method">> => <<"PUT">>, <<"body">> => ReqBody}),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req1),
    {ok, Req, put};
init(Req0, delete) ->
    Body = jsx:encode(#{<<"method">> => <<"DELETE">>}),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req0),
    {ok, Req, delete};
init(Req0, status) ->
    Code = binary_to_integer(cowboy_req:binding(code, Req0)),
    Req = cowboy_req:reply(Code, #{}, <<>>, Req0),
    {ok, Req, status};
init(Req0, json) ->
    Body = jsx:encode(#{<<"type">> => <<"json">>, <<"value">> => 42}),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req0),
    {ok, Req, json};
init(Req0, echo_headers) ->
    Headers = cowboy_req:headers(Req0),
    Body = jsx:encode(Headers),
    Req = cowboy_req:reply(200, ?JSON_CT, Body, Req0),
    {ok, Req, echo_headers}.
