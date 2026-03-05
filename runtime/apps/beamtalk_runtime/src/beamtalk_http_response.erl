%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc HTTPResponse instance dispatch — sealed value class for HTTP responses.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% HTTPResponse instances are tagged maps produced by `beamtalk_http` request
%%% functions. This module handles instance-method dispatch for the
%%% `HTTPResponse` class.
%%%
%%% ## Internal Representation
%%%
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'HTTPResponse',
%%%   status  => integer(),
%%%   headers => [[binary(), binary()]],
%%%   body    => binary()
%%% }
%%% ```
%%%
%%% ## Selectors
%%%
%%% | Selector      | Description                                    |
%%% |---------------|------------------------------------------------|
%%% | `status`      | HTTP status code (integer)                     |
%%% | `headers`     | List of [name, value] binary pairs             |
%%% | `body`        | Response body (binary / String)                |
%%% | `ok`          | True for 2xx status codes                      |
%%% | `bodyAsJson`  | Parse body via beamtalk_json                   |

-module(beamtalk_http_response).

-export([dispatch/3, has_method/1]).
-export([
    status/1,
    headers/1,
    body/1,
    ok/1,
    'bodyAsJson'/1
]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/3 + has_method/1 (compiled stdlib interface)
%%% ============================================================================

%% @doc Dispatch an instance message to an HTTPResponse tagged map.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('status', [], Self) ->
    status(Self);
dispatch('headers', [], Self) ->
    headers(Self);
dispatch('body', [], Self) ->
    body(Self);
dispatch('ok', [], Self) ->
    ok(Self);
dispatch('bodyAsJson', [], Self) ->
    'bodyAsJson'(Self);
dispatch('class', _Args, _Self) ->
    'HTTPResponse';
dispatch('printString', _Args, Self) ->
    print_string(Self);
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
                    'HTTPResponse',
                    Selector,
                    <<"HTTPResponse does not understand this message">>
                )
            )
    end.

%% @doc Check if HTTPResponse responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('status') -> true;
has_method('headers') -> true;
has_method('body') -> true;
has_method('ok') -> true;
has_method('bodyAsJson') -> true;
has_method(Selector) -> beamtalk_object_ops:has_method(Selector).

%%% ============================================================================
%%% Instance methods
%%% ============================================================================

%% @doc Return the HTTP status code.
-spec status(map()) -> integer().
status(#{'$beamtalk_class' := 'HTTPResponse', status := Status}) ->
    Status.

%% @doc Return the response headers as a list of [name, value] pairs.
-spec headers(map()) -> list().
headers(#{'$beamtalk_class' := 'HTTPResponse', headers := Headers}) ->
    Headers.

%% @doc Return the response body as a binary (String).
-spec body(map()) -> binary().
body(#{'$beamtalk_class' := 'HTTPResponse', body := Body}) ->
    Body.

%% @doc Return true if status is in the 2xx success range.
-spec ok(map()) -> boolean().
ok(#{'$beamtalk_class' := 'HTTPResponse', status := Status}) ->
    Status >= 200 andalso Status =< 299.

%% @doc Parse the response body as JSON via beamtalk_json.
-spec 'bodyAsJson'(map()) -> term().
'bodyAsJson'(#{'$beamtalk_class' := 'HTTPResponse', body := Body}) ->
    beamtalk_json:'parse:'(Body).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Human-readable representation.
-spec print_string(map()) -> binary().
print_string(#{'$beamtalk_class' := 'HTTPResponse', status := Status}) ->
    iolist_to_binary([<<"an HTTPResponse(">>, integer_to_binary(Status), <<")">>]).
