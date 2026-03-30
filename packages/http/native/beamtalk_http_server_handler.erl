%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Cowboy handler that bridges HTTP requests to Beamtalk handlers (BT-1338).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Each incoming HTTP request is converted to an `HTTPRequest` value object
%%% and passed to the user-supplied handler. The handler may be:
%%%
%%% - A block (Erlang `fun/1`) receiving an `HTTPRequest`
%%% - An actor pid responding to `handle:` with an `HTTPRequest`
%%%
%%% The handler must return an `HTTPResponse` value object. The response's
%%% `status`, `headers`, and `body` fields are extracted and sent back to
%%% the client via cowboy.

-module(beamtalk_http_server_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

%% @doc Handle a cowboy HTTP request by delegating to the Beamtalk handler.
%%
%% State is `#{handler := Handler}` where Handler is a fun/1, actor pid,
%% or an HTTPRouter Value object (map with `compiledRoutes` key, BT-1344).
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, #{handler := Handler} = State) ->
    {HttpRequest, Req1} = build_request(Req0),
    try dispatch_handler(Handler, HttpRequest) of
        Response when is_map(Response) ->
            Status = get_response_field(Response, status, 200),
            Headers = get_response_headers(Response),
            Body = get_response_field(Response, body, <<>>),
            Req = cowboy_req:reply(Status, Headers, Body, Req1),
            {ok, Req, State};
        Other ->
            ?LOG_ERROR("HTTP handler returned non-HTTPResponse", #{
                value => Other,
                domain => [beamtalk, stdlib]
            }),
            Req = cowboy_req:reply(
                500,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Internal Server Error">>,
                Req1
            ),
            {ok, Req, State}
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR("HTTP handler crashed", #{
                class => Class,
                reason => Reason,
                stacktrace => Stack,
                domain => [beamtalk, stdlib]
            }),
            Req = cowboy_req:reply(
                500,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Internal Server Error">>,
                Req1
            ),
            {ok, Req, State}
    end.

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% @private Build an HTTPRequest value object from a cowboy request.
%%
%% Returns `{HTTPRequest, UpdatedReq}` — the updated Req must be used for
%% the reply, since `read_body` consumes the body stream from the socket.
-dialyzer({nowarn_function, build_request/1}).
-spec build_request(cowboy_req:req()) -> {map(), cowboy_req:req()}.
build_request(Req0) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    HeadersList = maps:to_list(cowboy_req:headers(Req0)),
    Headers = [[Name, Value] || {Name, Value} <- HeadersList],
    {Body, Req1} = read_body(Req0),
    QsMap = parse_query_params(Req1),
    HttpRequest = 'bt@stdlib@httprequest':'class_method:path:headers:body:queryParams:params:'(
        undefined, undefined, Method, Path, Headers, Body, QsMap, #{}
    ),
    {HttpRequest, Req1}.

%% @private Read the full request body (may be chunked).
%%
%% Returns `{Body, UpdatedReq}` so the caller can use the updated Req
%% for subsequent cowboy operations (e.g. reply).
-spec read_body(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
read_body(Req) ->
    read_body(Req, <<>>).

-spec read_body(cowboy_req:req(), binary()) -> {binary(), cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {<<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

%% @private Parse query string into a Beamtalk Dictionary (Erlang map).
-spec parse_query_params(cowboy_req:req()) -> map().
parse_query_params(Req) ->
    QsList = cowboy_req:parse_qs(Req),
    maps:from_list([{Key, value_or_true(Val)} || {Key, Val} <- QsList]).

%% @private Query params without a value (e.g. "?flag") get `true`.
-spec value_or_true(binary() | true) -> binary() | true.
value_or_true(true) -> <<"true">>;
value_or_true(Val) -> Val.

%% @private Dispatch the request to the appropriate handler.
%%
%% Supports three handler types:
%%   - HTTPRouter Value object (map with `compiledRoutes` key) — compiled router (BT-1344)
%%   - `fun/1` — block handler
%%   - `pid()` — actor responding to `handle:`
-spec dispatch_handler(term(), map()) -> term().
dispatch_handler(#{compiledRoutes := Routes, notFoundHandler := NotFoundHandler}, Request) ->
    dispatch_router(Routes, NotFoundHandler, Request);
dispatch_handler(Handler, Request) ->
    call_handler(Handler, Request).

%% @private Route dispatch: match method+path, inject params, call handler.
-spec dispatch_router(list(), term(), map()) -> term().
dispatch_router(Routes, NotFoundHandler, Request) ->
    Method = maps:get(method, Request),
    Path = maps:get(path, Request),
    case beamtalk_http_router:match(Routes, Method, Path) of
        {ok, Handler, Params} ->
            EnrichedRequest = Request#{params => Params},
            call_handler(Handler, EnrichedRequest);
        method_not_allowed ->
            make_error_response(405, <<"Method Not Allowed">>);
        not_found when NotFoundHandler =:= nil; NotFoundHandler =:= undefined ->
            make_error_response(404, <<"Not Found">>);
        not_found ->
            call_handler(NotFoundHandler, Request)
    end.

%% @private Call the handler with the request.
%%
%% Supports blocks (funs) and actor pids responding to `handle:`.
-spec call_handler(fun((map()) -> map()) | pid(), map()) -> term().
call_handler(Handler, Request) when is_function(Handler, 1) ->
    Handler(Request);
call_handler(Handler, Request) when is_pid(Handler) ->
    beamtalk_actor:sync_send(Handler, 'handle:', [Request]).

%% @private Build a simple HTTPResponse map for error responses.
-spec make_error_response(integer(), binary()) -> map().
make_error_response(Status, Body) ->
    #{
        '$beamtalk_class' => 'HTTPResponse',
        status => Status,
        headers => [[<<"content-type">>, <<"text/plain">>]],
        body => Body
    }.

%% @private Extract a field from an HTTPResponse value object.
%%
%% HTTPResponse objects store fields in their map under the field name atom.
%% The generated accessor methods use these keys.
-spec get_response_field(map(), atom(), term()) -> term().
get_response_field(Response, Field, Default) ->
    maps:get(Field, Response, Default).

%% @private Convert HTTPResponse headers to cowboy headers map.
%%
%% HTTPResponse headers are `[[Name, Value], ...]`.
%% Cowboy expects `#{Name => Value, ...}`.
-spec get_response_headers(map()) -> map().
get_response_headers(Response) ->
    case maps:get(headers, Response, []) of
        Headers when is_list(Headers) ->
            maps:from_list([{Name, Value} || [Name, Value] <- Headers]);
        _ ->
            #{}
    end.
