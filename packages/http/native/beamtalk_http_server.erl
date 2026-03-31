%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Native gen_server backing the HTTPServer actor class (BT-1338, ADR 0056).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Manages cowboy listener lifecycle for the `HTTPServer` Actor class.
%%% The compiled facade module (`bt@http@httpserver`) handles class/instance
%%% dispatch; this module exports `start_link/1` and gen_server callbacks.
%%%
%%% Each HTTPServer actor owns a cowboy listener identified by a unique
%%% reference. The listener dispatches all requests to a single Beamtalk
%%% handler (block or actor).
%%%
%%% == State Map ==
%%%
%%% ```erlang
%%% #{
%%%   listener_ref => reference(),      % cowboy listener ref
%%%   actual_port  => non_neg_integer() % TCP port the server is listening on
%%% }
%%% ```
%%%
%%% == Selectors ==
%%%
%%% * `{port, []}` — return the TCP port the server is listening on
%%% * `{printString, []}` — human-readable representation

-module(beamtalk_http_server).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Native backing gen_server API (ADR 0056)
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start a linked HTTPServer gen_server (ADR 0056).
%%
%% Config must contain `port` (integer) and `handler` (fun/1, pid, or HTTPRouter).
%% Optional: `bind` (IP address binary, default `"127.0.0.1"`).
%%
%% Called by the native facade's `spawn/1` via `spawnWith:`.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

%% @doc Start the cowboy listener on init.
-spec init(map()) -> {ok, map()} | {stop, term()}.
init(Config) when is_map(Config) ->
    Port = maps:get(port, Config, 0),
    Handler = maps:get(handler, Config, undefined),
    Opts = maps:without([port, handler, '$beamtalk_class'], Config),
    try start_listener(Port, Handler, Opts) of
        {Ref, ActualPort} ->
            {ok, #{listener_ref => Ref, actual_port => ActualPort}}
    catch
        error:Err ->
            {stop, Err}
    end;
init(_) ->
    {stop, bad_config}.

%% @doc Dispatch sync calls from `self delegate` methods.
-spec handle_call(term(), term(), map()) ->
    {reply, term(), map()}.
%% BT-1604: Strip propagated context from 3-tuple messages (ADR 0069 Phase 2b)
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);
handle_call({port, []}, _From, #{actual_port := ActualPort} = State) ->
    {reply, ActualPort, State};
handle_call({printString, []}, _From, #{actual_port := ActualPort} = State) ->
    Str = iolist_to_binary([
        <<"an HTTPServer(port: ">>,
        integer_to_binary(ActualPort),
        <<")">>
    ]),
    {reply, Str, State};
handle_call(Msg, _From, State) ->
    ?LOG_WARNING("Unknown call", #{message => Msg}),
    {reply, {error, unknown_call}, State}.

%% @doc Ignore casts.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc No-op — cowboy manages its own processes.
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Msg, State) ->
    {noreply, State}.

%% @doc Stop the cowboy listener on shutdown.
-spec terminate(term(), map()) -> ok.
terminate(_Reason, #{listener_ref := Ref}) ->
    stop_listener(Ref),
    ok;
terminate(_Reason, _State) ->
    ok.

%%% ============================================================================
%%% Internal — cowboy lifecycle
%%% ============================================================================

%% @private Start a cowboy listener on the given port with a handler.
%%
%% Called from init/1. Raises on validation or startup errors (caught by init).
-dialyzer({nowarn_function, start_listener/3}).
-spec start_listener(non_neg_integer(), fun() | pid(), map()) ->
    {reference(), non_neg_integer()}.
start_listener(Port, Handler, Opts) when is_integer(Port), Port >= 0, is_map(Opts) ->
    validate_handler(Handler),
    ensure_started(cowboy),
    %% Start gun too — HTTPClient needs it and BUnit tests don't start it automatically
    ensure_started(gun),
    Bind = maps:get(bind, Opts, <<"127.0.0.1">>),
    IpTuple = parse_ip(Bind),
    Ref = make_ref(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', beamtalk_http_server_handler, #{handler => Handler}}
        ]}
    ]),
    TransOpts = [{port, Port}, {ip, IpTuple}],
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(Ref, TransOpts, ProtoOpts) of
        {ok, _Pid} ->
            ActualPort = ranch:get_port(Ref),
            {Ref, ActualPort};
        {error, Reason} ->
            server_error(
                'start:handler:',
                #{port => Port, reason => Reason},
                <<"Failed to start HTTP server">>
            )
    end;
start_listener(Port, _Handler, _Opts) when is_integer(Port), Port >= 0 ->
    type_error('start:handler:options:', <<"Options must be a Dictionary">>);
start_listener(Port, _Handler, _Opts) when not is_integer(Port) ->
    type_error('start:handler:', <<"Port must be an Integer">>);
start_listener(Port, _Handler, _Opts) when Port < 0 ->
    type_error('start:handler:', <<"Port must be a non-negative Integer">>).

%% @private Stop a cowboy listener identified by its reference.
%%
%% Idempotent: stopping an already-stopped server succeeds silently.
-spec stop_listener(reference()) -> ok.
stop_listener(Ref) when is_reference(Ref) ->
    _ = catch cowboy:stop_listener(Ref),
    ok;
stop_listener(_) ->
    ok.

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% @private Ensure an OTP application is started, raising a structured error on failure.
-spec ensure_started(atom()) -> ok | no_return().
ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            server_error(
                'start:handler:',
                #{application => App, reason => Reason},
                iolist_to_binary([
                    <<"Failed to start dependency: ">>,
                    atom_to_binary(App, utf8)
                ])
            )
    end.

%% @private Validate that the handler is a fun/1, a pid, or an HTTPRouter value.
-spec validate_handler(term()) -> ok | no_return().
validate_handler(Handler) when is_function(Handler, 1) -> ok;
validate_handler(Handler) when is_pid(Handler) -> ok;
validate_handler(#{compiledRoutes := Routes, notFoundHandler := _}) when is_list(Routes) -> ok;
validate_handler(_) ->
    type_error(
        'start:handler:',
        <<"Handler must be a block, an actor, or an HTTPRouter">>
    ).

%% @private Parse an IP address string to a tuple.
-spec parse_ip(term()) -> inet:ip_address().
parse_ip(Bin) when is_binary(Bin) ->
    case inet:parse_address(binary_to_list(Bin)) of
        {ok, Ip} ->
            Ip;
        {error, _} ->
            type_error(
                'start:handler:options:',
                <<"Invalid bind address: must be a valid IP address">>
            )
    end;
parse_ip(_) ->
    type_error('start:handler:options:', <<"Bind must be a String">>).

%% @private Raise a type error.
-spec type_error(atom(), binary()) -> no_return().
type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'HTTPServer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @private Raise a server error.
-spec server_error(atom(), map(), binary()) -> no_return().
server_error(Selector, Details, Message) ->
    Error0 = beamtalk_error:new(http_error, 'HTTPServer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, Details),
    Error3 = beamtalk_error:with_hint(Error2, Message),
    beamtalk_error:raise(Error3).
