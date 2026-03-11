%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc HTTPServer lifecycle management — cowboy wrapper for Beamtalk (BT-1338).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Manages cowboy listener lifecycle for `HTTPServer` stdlib class.
%%% Each HTTPServer instance owns a cowboy listener identified by a unique
%%% reference. The listener dispatches all requests to a single Beamtalk
%%% handler (block or actor).
%%%
%%% ## Selectors
%%%
%%% | Selector                                  | Description                      |
%%% |-------------------------------------------|----------------------------------|
%%% | `start: port handler: handler`            | Start server, bind to 127.0.0.1  |
%%% | `start: port handler: handler options: o`  | Start with options               |
%%% | `stop: ref`                               | Stop a running server            |
%%% | `port: ref`                               | Get the bound port               |

-module(beamtalk_http_server).

-export([dispatch/3, has_method/1]).
-export([start/2, start/3, stop/1, port/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% dispatch/3 + has_method/1 (compiled stdlib interface)
%%% ============================================================================

%% @doc Dispatch a message to the HTTPServer class object.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('start:handler:', [Port, Handler], _Self) ->
    start(Port, Handler);
dispatch('start:handler:options:', [Port, Handler, Opts], _Self) ->
    start(Port, Handler, Opts);
dispatch('class', _Args, _Self) ->
    'HTTPServer';
dispatch('printString', _Args, _Self) ->
    <<"HTTPServer">>;
dispatch(Selector, Args, Self) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    does_not_understand,
                    'HTTPServer',
                    Selector,
                    <<"HTTPServer does not understand this message">>
                )
            )
    end.

%% @doc Check if HTTPServer class responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('start:handler:') -> true;
has_method('start:handler:options:') -> true;
has_method(Selector) -> beamtalk_object_ops:has_method(Selector).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start an HTTP server on the given port with a handler.
%%
%% `Port` is an integer. Use 0 for an ephemeral port.
%% `Handler` is a fun/1 (block) or actor pid responding to `handle:`.
%% Binds to 127.0.0.1 by default.
%%
%% Returns a `Result` wrapping an HTTPServer object.
-spec start(non_neg_integer(), fun() | pid()) -> map().
start(Port, Handler) ->
    start(Port, Handler, #{}).

%% @doc Start an HTTP server with options.
%%
%% Options:
%%   - `#bind` — IP address to bind to (default `"127.0.0.1"`)
-dialyzer({nowarn_function, start/3}).
-spec start(non_neg_integer(), fun() | pid(), map()) -> map().
start(Port, Handler, Opts) when is_integer(Port), Port >= 0, is_map(Opts) ->
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
            beamtalk_result:from_tagged_tuple({ok, make_server(Ref, ActualPort)});
        {error, Reason} ->
            Error = beamtalk_error:new(http_error, 'HTTPServer'),
            Error1 = beamtalk_error:with_selector(Error, 'start:handler:options:'),
            Error2 = beamtalk_error:with_details(Error1, #{port => Port, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Failed to start HTTP server">>),
            ExObj = beamtalk_exception_handler:ensure_wrapped(Error3),
            beamtalk_result:from_tagged_tuple({error, ExObj})
    end;
start(Port, _Handler, _Opts) when is_integer(Port), Port >= 0 ->
    type_error('start:handler:options:', <<"Options must be a Dictionary">>);
start(Port, _Handler, _Opts) when not is_integer(Port) ->
    type_error('start:handler:', <<"Port must be an Integer">>);
start(Port, _Handler, _Opts) when Port < 0 ->
    type_error('start:handler:', <<"Port must be a non-negative Integer">>).

%% @doc Stop an HTTP server identified by its listener reference.
%%
%% Idempotent: stopping an already-stopped server succeeds silently.
-spec stop(reference()) -> atom().
stop(Ref) when is_reference(Ref) ->
    _ = catch cowboy:stop_listener(Ref),
    ok;
stop(_) ->
    type_error('stop', <<"Expected an HTTPServer listener reference">>).

%% @doc Return the port of a running HTTP server.
-spec port(reference()) -> non_neg_integer().
port(Ref) when is_reference(Ref) ->
    try
        ranch:get_port(Ref)
    catch
        exit:{noproc, _} ->
            server_error('port', #{}, <<"Server is not running">>)
    end;
port(_) ->
    type_error('port', <<"Expected an HTTPServer listener reference">>).

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% @private Validate that the handler is a fun/1 or a pid.
-spec validate_handler(term()) -> ok | no_return().
validate_handler(Handler) when is_function(Handler, 1) -> ok;
validate_handler(Handler) when is_pid(Handler) -> ok;
validate_handler(_) ->
    type_error('start:handler:', <<"Handler must be a block or an actor responding to handle:">>).

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

%% @private Build an HTTPServer object.
-dialyzer({nowarn_function, make_server/2}).
-spec make_server(reference(), non_neg_integer()) -> map().
make_server(Ref, ActualPort) ->
    'bt@stdlib@httpserver':'new'(#{listenerRef => Ref, actualPort => ActualPort}).

%% @private Raise a type error.
-spec type_error(atom(), binary()) -> no_return().
type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'HTTPServer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @private Return a Result error for a server failure.
-spec server_error(atom(), map(), binary()) -> no_return().
server_error(Selector, Details, Message) ->
    Error0 = beamtalk_error:new(http_error, 'HTTPServer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, Details),
    Error3 = beamtalk_error:with_hint(Error2, Message),
    beamtalk_error:raise(Error3).
