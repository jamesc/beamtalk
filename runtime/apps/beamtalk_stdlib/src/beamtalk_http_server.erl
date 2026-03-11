%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc HTTPServer lifecycle management — cowboy wrapper for Beamtalk (BT-1338).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Manages cowboy listener lifecycle for `HTTPServer` Actor class.
%%% Called from the actor's `initialize` and `terminate` methods.
%%% Each HTTPServer actor owns a cowboy listener identified by a unique
%%% reference. The listener dispatches all requests to a single Beamtalk
%%% handler (block or actor).

-module(beamtalk_http_server).

-export([startListener/2, startListener/3, stopListener/1, listenerPort/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API (called from HTTPServer actor methods)
%%% ============================================================================

%% @doc Start a cowboy listener on the given port with a handler.
%%
%% `Port` is an integer. Use 0 for an ephemeral port.
%% `Handler` is a fun/1 (block) or actor pid responding to `handle:`.
%% Binds to 127.0.0.1 by default.
%%
%% Returns `{Ref, ActualPort}` as a tuple (Erlang list for Beamtalk).
-dialyzer({nowarn_function, startListener/2}).
-spec startListener(non_neg_integer(), fun() | pid()) -> list().
startListener(Port, Handler) ->
    startListener(Port, Handler, #{}).

%% @doc Start a cowboy listener with options.
%%
%% Options:
%%   - `bind` — IP address to bind to (default `"127.0.0.1"`)
-dialyzer({nowarn_function, startListener/3}).
-spec startListener(non_neg_integer(), fun() | pid(), map()) -> list().
startListener(Port, Handler, Opts) when is_integer(Port), Port >= 0, is_map(Opts) ->
    validate_handler(Handler),
    {ok, _} = application:ensure_all_started(cowboy),
    %% Start gun too — HTTPClient needs it and BUnit tests don't start it automatically
    {ok, _} = application:ensure_all_started(gun),
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
            [Ref, ActualPort];
        {error, Reason} ->
            server_error(
                'startListener:handler:',
                #{port => Port, reason => Reason},
                <<"Failed to start HTTP server">>
            )
    end;
startListener(Port, _Handler, _Opts) when is_integer(Port), Port >= 0 ->
    type_error('startListener:handler:options:', <<"Options must be a Dictionary">>);
startListener(Port, _Handler, _Opts) when not is_integer(Port) ->
    type_error('startListener:handler:', <<"Port must be an Integer">>);
startListener(Port, _Handler, _Opts) when Port < 0 ->
    type_error('startListener:handler:', <<"Port must be a non-negative Integer">>).

%% @doc Stop a cowboy listener identified by its reference.
%%
%% Idempotent: stopping an already-stopped server succeeds silently.
-spec stopListener(reference()) -> atom().
stopListener(Ref) when is_reference(Ref) ->
    _ = catch cowboy:stop_listener(Ref),
    ok;
stopListener(_) ->
    ok.

%% @doc Return the port of a running cowboy listener.
-spec listenerPort(reference()) -> non_neg_integer().
listenerPort(Ref) when is_reference(Ref) ->
    try
        ranch:get_port(Ref)
    catch
        exit:{noproc, _} ->
            server_error('listenerPort:', #{}, <<"Server is not running">>)
    end;
listenerPort(_) ->
    type_error('listenerPort:', <<"Expected a listener reference">>).

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% @private Validate that the handler is a fun/1 or a pid.
-spec validate_handler(term()) -> ok | no_return().
validate_handler(Handler) when is_function(Handler, 1) -> ok;
validate_handler(Handler) when is_pid(Handler) -> ok;
validate_handler(_) ->
    type_error(
        'startListener:handler:',
        <<"Handler must be a block or an actor responding to handle:">>
    ).

%% @private Parse an IP address string to a tuple.
-spec parse_ip(term()) -> inet:ip_address().
parse_ip(Bin) when is_binary(Bin) ->
    case inet:parse_address(binary_to_list(Bin)) of
        {ok, Ip} ->
            Ip;
        {error, _} ->
            type_error(
                'startListener:handler:options:',
                <<"Invalid bind address: must be a valid IP address">>
            )
    end;
parse_ip(_) ->
    type_error('startListener:handler:options:', <<"Bind must be a String">>).

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
