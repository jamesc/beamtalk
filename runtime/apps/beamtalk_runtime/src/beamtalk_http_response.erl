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
%%% | `bodyAsJson/1`  | Parse body as JSON (jsx), null → nil           |
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

%% @doc Parse the response body as JSON.
%%
%% Decodes via jsx. null becomes nil to match Beamtalk conventions.
%% Raises parse_error if the body is not valid JSON.
-spec 'bodyAsJson'(map()) -> term().
'bodyAsJson'(#{'$beamtalk_class' := 'HTTPResponse', body := Body}) ->
    try jsx:decode(Body, [return_maps]) of
        Result ->
            normalize_null(Result)
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        error:badarg ->
            Error0 = beamtalk_error:new(parse_error, 'HTTPResponse'),
            Error1 = beamtalk_error:with_selector(Error0, 'bodyAsJson'),
            Error2 = beamtalk_error:with_hint(
                Error1, <<"Check that the response body is valid JSON">>
            ),
            beamtalk_error:raise(Error2);
        error:Reason ->
            Error0 = beamtalk_error:new(parse_error, 'HTTPResponse'),
            Error1 = beamtalk_error:with_selector(Error0, 'bodyAsJson'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(
                Error2, <<"Check that the response body is valid JSON">>
            ),
            beamtalk_error:raise(Error3)
    end.

%% @private Convert jsx null atoms to Beamtalk nil.
-spec normalize_null(term()) -> term().
normalize_null(null) -> nil;
normalize_null(Map) when is_map(Map) -> maps:map(fun(_, V) -> normalize_null(V) end, Map);
normalize_null(List) when is_list(List) -> lists:map(fun normalize_null/1, List);
normalize_null(Other) -> Other.

%% @doc Human-readable representation: "an HTTPResponse(200)".
-spec 'printString'(map()) -> binary().
'printString'(#{'$beamtalk_class' := 'HTTPResponse', status := Status}) ->
    iolist_to_binary([<<"an HTTPResponse(">>, integer_to_binary(Status), <<")">>]).
