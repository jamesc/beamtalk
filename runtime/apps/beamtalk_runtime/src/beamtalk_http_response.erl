%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc HTTPResponse accessors — sealed value class for HTTP responses.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% HTTPResponse instances are tagged maps produced by `beamtalk_http` request
%%% functions. This module exposes direct accessor functions called via the
%%% Erlang FFI layer from `HTTPResponse.bt`.
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
%%% ## Exported Accessors
%%%
%%% | Function      | Description                                    |
%%% |---------------|------------------------------------------------|
%%% | `status/1`    | HTTP status code (integer)                     |
%%% | `headers/1`   | List of [name, value] binary pairs             |
%%% | `body/1`      | Response body (binary / String)                |
%%% | `ok/1`        | True for 2xx status codes                      |
%%% | `bodyAsJson/1`  | Parse body via beamtalk_json                   |
%%% | `printString/1` | Human-readable representation                  |

-module(beamtalk_http_response).

-export([
    status/1,
    headers/1,
    body/1,
    ok/1,
    'bodyAsJson'/1,
    'printString'/1
]).

-include("beamtalk.hrl").

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

%% @doc Human-readable representation: "an HTTPResponse(200)".
-spec 'printString'(map()) -> binary().
'printString'(#{'$beamtalk_class' := 'HTTPResponse', status := Status}) ->
    iolist_to_binary([<<"an HTTPResponse(">>, integer_to_binary(Status), <<")">>]).
