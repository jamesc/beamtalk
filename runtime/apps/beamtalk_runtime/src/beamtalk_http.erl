%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc HTTP class-side primitive wrapping gun (BT-1114).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Implements HTTP client operations for the `Http` class object.
%%% Uses gun 2.x for HTTP/1.1 and HTTP/2 with synchronous await pattern.
%%% HTTPS is enabled automatically when the URL scheme is `https`.
%%%
%%% ## Selectors
%%%
%%% | Selector                         | Description                          |
%%% |----------------------------------|--------------------------------------|
%%% | `get: url`                       | GET with no headers                  |
%%% | `get: url headers: headers`      | GET with headers                     |
%%% | `post: url body: body`           | POST with body, no extra headers     |
%%% | `post: url headers: h body: b`   | POST with headers and body           |
%%% | `put: url body: body`            | PUT with body, no extra headers      |
%%% | `put: url headers: h body: b`    | PUT with headers and body            |
%%% | `delete: url`                    | DELETE with no headers               |
%%% | `delete: url headers: headers`   | DELETE with headers                  |
%%% | `request: method url: url options: opts` | Generic request          |
%%%
%%% ## Response Format
%%%
%%% All requests return an `HTTPResponse` value object constructed via the
%%% generated keyword constructor `'bt@stdlib@httpresponse':'class_status:headers:body:'/5`.
%%% Fields: `status` (integer), `headers` (list of [Name, Value] binary pairs),
%%% `body` (String).
%%%
%%% ## Options map (for `request:url:options:`)
%%%
%%% | Key       | Type             | Default  | Description              |
%%% |-----------|------------------|----------|--------------------------|
%%% | `headers` | list of [K, V]   | `[]`     | Request headers          |
%%% | `body`    | binary           | `<<>>`   | Request body             |
%%% | `timeout` | integer (ms)     | `30000`  | Total request timeout    |

-module(beamtalk_http).

-export([dispatch/3, has_method/1]).
-export([
    'get:'/1,
    'get:headers:'/2,
    'post:body:'/2,
    'post:headers:body:'/3,
    'put:body:'/2,
    'put:headers:body:'/3,
    'delete:'/1,
    'delete:headers:'/2,
    'request:url:options:'/3
]).

%% No-colon aliases for Beamtalk proxy dispatch (BT-1117).
%%
%% The Beamtalk codegen routes `(Erlang M) keyword: arg` through
%% `beamtalk_erlang_proxy:direct_call(M, stripped_fun, args)` where
%% `stripped_fun` has the trailing colon removed (e.g. `get:` → `get`).
%% These delegates expose the colon-style implementations under the
%% expected no-colon names so `(Erlang beamtalk_http) get: url` works.
-export([get/1, post/2, put/2, delete/1, request/3]).
%% Exported for unit testing (beamtalk_http_tests)
-export([
    normalise_method/1,
    validate_request_options/3,
    to_gun_headers/2,
    from_gun_headers/1,
    parse_url/1
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_TIMEOUT, 30000).

%%% ============================================================================
%%% dispatch/3 + has_method/1 (compiled stdlib interface)
%%% ============================================================================

%% @doc Dispatch a message to the Http class object.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('get:', [Url], _Self) ->
    'get:'(Url);
dispatch('get:headers:', [Url, Headers], _Self) ->
    'get:headers:'(Url, Headers);
dispatch('post:body:', [Url, Body], _Self) ->
    'post:body:'(Url, Body);
dispatch('post:headers:body:', [Url, Headers, Body], _Self) ->
    'post:headers:body:'(Url, Headers, Body);
dispatch('put:body:', [Url, Body], _Self) ->
    'put:body:'(Url, Body);
dispatch('put:headers:body:', [Url, Headers, Body], _Self) ->
    'put:headers:body:'(Url, Headers, Body);
dispatch('delete:', [Url], _Self) ->
    'delete:'(Url);
dispatch('delete:headers:', [Url, Headers], _Self) ->
    'delete:headers:'(Url, Headers);
dispatch('request:url:options:', [Method, Url, Options], _Self) ->
    'request:url:options:'(Method, Url, Options);
dispatch('class', _Args, _Self) ->
    'Http';
dispatch('printString', _Args, _Self) ->
    <<"Http">>;
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
                    'Http',
                    Selector,
                    <<"Http does not understand this message">>
                )
            )
    end.

%% @doc Check if Http class responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('get:') -> true;
has_method('get:headers:') -> true;
has_method('post:body:') -> true;
has_method('post:headers:body:') -> true;
has_method('put:body:') -> true;
has_method('put:headers:body:') -> true;
has_method('delete:') -> true;
has_method('delete:headers:') -> true;
has_method('request:url:options:') -> true;
has_method(Selector) -> beamtalk_object_ops:has_method(Selector).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Perform a GET request with no extra headers.
-spec 'get:'(binary()) -> map().
'get:'(Url) ->
    'get:headers:'(Url, []).

%% @doc Perform a GET request.
%%
%% `Headers` is a list of `[Name, Value]` binary pairs.
%% Returns an `HTTPResponse` tagged map.
-spec 'get:headers:'(binary(), list()) -> map().
'get:headers:'(Url, Headers) when is_binary(Url), is_list(Headers) ->
    do_request(<<"GET">>, Url, Headers, <<>>, ?DEFAULT_TIMEOUT, 'get:headers:');
'get:headers:'(_, Headers) when not is_list(Headers) ->
    type_error('get:headers:', <<"Headers must be a List">>);
'get:headers:'(_, _) ->
    type_error('get:headers:', <<"Url must be a String">>).

%% @doc Perform a POST request with body and no extra headers.
-spec 'post:body:'(binary(), binary()) -> map().
'post:body:'(Url, Body) ->
    'post:headers:body:'(Url, [], Body).

%% @doc Perform a POST request with headers and body.
-spec 'post:headers:body:'(binary(), list(), binary()) -> map().
'post:headers:body:'(Url, Headers, Body) when is_binary(Url), is_list(Headers), is_binary(Body) ->
    do_request(<<"POST">>, Url, Headers, Body, ?DEFAULT_TIMEOUT, 'post:headers:body:');
'post:headers:body:'(Url, _, _) when not is_binary(Url) ->
    type_error('post:headers:body:', <<"Url must be a String">>);
'post:headers:body:'(_, Headers, _) when not is_list(Headers) ->
    type_error('post:headers:body:', <<"Headers must be a List">>);
'post:headers:body:'(_, _, Body) when not is_binary(Body) ->
    type_error('post:headers:body:', <<"Body must be a String">>).

%% @doc Perform a PUT request with body and no extra headers.
-spec 'put:body:'(binary(), binary()) -> map().
'put:body:'(Url, Body) ->
    'put:headers:body:'(Url, [], Body).

%% @doc Perform a PUT request with headers and body.
-spec 'put:headers:body:'(binary(), list(), binary()) -> map().
'put:headers:body:'(Url, Headers, Body) when is_binary(Url), is_list(Headers), is_binary(Body) ->
    do_request(<<"PUT">>, Url, Headers, Body, ?DEFAULT_TIMEOUT, 'put:headers:body:');
'put:headers:body:'(Url, _, _) when not is_binary(Url) ->
    type_error('put:headers:body:', <<"Url must be a String">>);
'put:headers:body:'(_, Headers, _) when not is_list(Headers) ->
    type_error('put:headers:body:', <<"Headers must be a List">>);
'put:headers:body:'(_, _, Body) when not is_binary(Body) ->
    type_error('put:headers:body:', <<"Body must be a String">>).

%% @doc Perform a DELETE request with no extra headers.
-spec 'delete:'(binary()) -> map().
'delete:'(Url) ->
    'delete:headers:'(Url, []).

%% @doc Perform a DELETE request.
-spec 'delete:headers:'(binary(), list()) -> map().
'delete:headers:'(Url, Headers) when is_binary(Url), is_list(Headers) ->
    do_request(<<"DELETE">>, Url, Headers, <<>>, ?DEFAULT_TIMEOUT, 'delete:headers:');
'delete:headers:'(_, Headers) when not is_list(Headers) ->
    type_error('delete:headers:', <<"Headers must be a List">>);
'delete:headers:'(_, _) ->
    type_error('delete:headers:', <<"Url must be a String">>).

%% @doc Perform a generic HTTP request.
%%
%% `Method` is a binary like `<<"GET">>` or a symbol like `#get`.
%% `Options` is a map with optional keys: `headers`, `body`, `timeout`.
-spec 'request:url:options:'(term(), binary(), map()) -> map().
'request:url:options:'(Method, Url, Options) when is_binary(Url), is_map(Options) ->
    MethodBin = normalise_method(Method),
    Headers = maps:get(headers, Options, []),
    Body = maps:get(body, Options, <<>>),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    validate_request_options(Headers, Body, Timeout),
    do_request(MethodBin, Url, Headers, Body, Timeout, 'request:url:options:');
'request:url:options:'(_, Url, _) when not is_binary(Url) ->
    type_error('request:url:options:', <<"Url must be a String">>);
'request:url:options:'(_, _, Options) when not is_map(Options) ->
    type_error('request:url:options:', <<"Options must be a Dictionary">>).

%%% ============================================================================
%%% No-colon delegates for Beamtalk proxy dispatch (BT-1117)
%%% ============================================================================

%% @doc GET delegate called by `(Erlang beamtalk_http) get: url` from Beamtalk.
-spec get(binary()) -> map().
get(Url) -> 'get:'(Url).

%% @doc POST delegate called by `(Erlang beamtalk_http) post: url body: body`.
-spec post(binary(), binary()) -> map().
post(Url, Body) -> 'post:body:'(Url, Body).

%% @doc PUT delegate called by `(Erlang beamtalk_http) put: url body: body`.
-spec put(binary(), binary()) -> map().
put(Url, Body) -> 'put:body:'(Url, Body).

%% @doc DELETE delegate called by `(Erlang beamtalk_http) delete: url`.
-spec delete(binary()) -> map().
delete(Url) -> 'delete:'(Url).

%% @doc Generic request delegate called by
%% `(Erlang beamtalk_http) request: method url: url options: opts`.
-spec request(term(), binary(), map()) -> map().
request(Method, Url, Options) -> 'request:url:options:'(Method, Url, Options).

%%% ============================================================================
%%% Internal implementation
%%% ============================================================================

%% @private Execute an HTTP request via gun.
%%
%% `Selector` is the public API selector, used in error reports so the user
%% sees `'get:'` rather than the internal `'do_request'` helper name.
-spec do_request(binary(), binary(), list(), binary(), non_neg_integer(), atom()) -> map().
do_request(Method, Url, BtHeaders, Body, Timeout, Selector) ->
    case parse_url(Url) of
        {error, invalid_url} ->
            http_error(Selector, #{url => Url}, <<"Invalid URL">>);
        {ok, #{host := Host, port := Port, path := Path, transport := Transport}} ->
            GunHeaders = to_gun_headers(BtHeaders, Selector),
            GunOpts = build_gun_opts(Transport, Host),
            Deadline = deadline(Timeout),
            case gun:open(Host, Port, GunOpts) of
                {ok, ConnPid} ->
                    MRef = monitor(process, ConnPid),
                    try
                        case gun:await_up(ConnPid, remaining(Deadline), MRef) of
                            {ok, _Protocol} ->
                                StreamRef = gun:request(
                                    ConnPid, Method, Path, GunHeaders, Body
                                ),
                                collect_response(
                                    ConnPid, StreamRef, MRef, Deadline, Selector
                                );
                            {error, {down, Reason}} ->
                                http_error(
                                    Selector,
                                    #{url => Url, reason => Reason},
                                    <<"Connection failed">>
                                );
                            {error, timeout} ->
                                http_error(
                                    Selector,
                                    #{url => Url},
                                    <<"Connection timed out">>
                                )
                        end
                    after
                        demonitor(MRef, [flush]),
                        gun:close(ConnPid)
                    end;
                {error, Reason} ->
                    http_error(
                        Selector,
                        #{url => Url, reason => Reason},
                        <<"Could not open connection">>
                    )
            end
    end.

%% @private Await response headers from gun, skipping 1xx informational responses.
%%
%% Gun 2.x may emit `{inform, ...}` for 1xx Informational responses (e.g. 100
%% Continue, 103 Early Hints) before the final response. We skip these and
%% recurse so the caller always sees the final `{response, ...}` event.
%%
%% Other unexpected event types (push, upgrade, ws, data, trailers) are logged
%% and skipped — they should not appear in a normal request/response flow but
%% must be handled to avoid case_clause crashes.
%%
%% `Deadline` is an absolute monotonic timestamp (ms) so the total wall-clock
%% time across all gun:await calls is bounded by the original timeout.
-spec collect_response(pid(), reference(), reference(), integer() | infinity, atom()) ->
    map().
collect_response(ConnPid, StreamRef, MRef, Deadline, Selector) ->
    case gun:await(ConnPid, StreamRef, remaining(Deadline), MRef) of
        {response, fin, Status, GunHeaders} ->
            %% No body (e.g. 204 No Content, HEAD)
            make_response(Status, GunHeaders, <<>>);
        {response, nofin, Status, GunHeaders} ->
            case gun:await_body(ConnPid, StreamRef, remaining(Deadline), MRef) of
                {ok, Body} ->
                    make_response(Status, GunHeaders, Body);
                {ok, Body, _Trailers} ->
                    make_response(Status, GunHeaders, Body);
                {error, timeout} ->
                    http_error(Selector, #{}, <<"Response body timed out">>);
                {error, Reason} ->
                    http_error(
                        Selector,
                        #{reason => Reason},
                        <<"Failed to read response body">>
                    )
            end;
        {inform, _Status, _Headers} ->
            %% 1xx Informational (e.g. 100 Continue, 103 Early Hints) — skip and wait
            collect_response(ConnPid, StreamRef, MRef, Deadline, Selector);
        {push, _PushedStreamRef, _Method, _URI, _Headers} ->
            %% HTTP/2 server push — not used, skip
            ?LOG_DEBUG("beamtalk_http: ignoring HTTP/2 server push", #{}),
            collect_response(ConnPid, StreamRef, MRef, Deadline, Selector);
        {upgrade, _Protocols, _Headers} ->
            %% Protocol upgrade (e.g. WebSocket) — unexpected in plain HTTP flow
            http_error(Selector, #{}, <<"Unexpected protocol upgrade">>);
        {ws, _Frame} ->
            %% WebSocket frame — unexpected in plain HTTP flow
            http_error(Selector, #{}, <<"Unexpected WebSocket frame">>);
        {data, _IsFin, _Data} ->
            %% Data frame before response headers — unexpected, skip
            ?LOG_DEBUG("beamtalk_http: ignoring unexpected data frame", #{}),
            collect_response(ConnPid, StreamRef, MRef, Deadline, Selector);
        {trailers, _Trailers} ->
            %% Trailers before response headers — unexpected, skip
            collect_response(ConnPid, StreamRef, MRef, Deadline, Selector);
        {error, timeout} ->
            http_error(Selector, #{}, <<"Request timed out">>);
        {error, Reason} ->
            http_error(Selector, #{reason => Reason}, <<"Request failed">>)
    end.

%% @private Build a Beamtalk HTTPResponse using the generated keyword constructor.
%%
%% Calls the constructor generated from `state:` declarations in `HTTPResponse.bt`.
%% This provides compile-time arity safety: if a field is added or removed from
%% HTTPResponse, the arity of this call changes and the compiler catches the mismatch.
%%
%% `bt@stdlib@httpresponse` lives in `beamtalk_stdlib`, which depends on
%% `beamtalk_runtime` and is therefore absent from the runtime Dialyzer PLT.
%% The suppression below silences the resulting "unknown function" warning;
%% the call is safe because the module is always loaded before HTTP requests
%% can be made (beamtalk_stdlib starts after beamtalk_runtime).
-dialyzer({nowarn_function, make_response/3}).
-spec make_response(non_neg_integer(), list(), binary()) -> map().
make_response(Status, GunHeaders, Body) ->
    BtHeaders = from_gun_headers(GunHeaders),
    'bt@stdlib@httpresponse':'class_status:headers:body:'(
        undefined, undefined, Status, BtHeaders, Body
    ).

%% @private Parse a URL binary into connection components.
%%
%% Returns `{ok, #{host, port, path, transport}}` or `{error, invalid_url}`.
-spec parse_url(binary()) -> {ok, map()} | {error, invalid_url}.
parse_url(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Uri when
            Scheme =:= <<"http">> orelse Scheme =:= <<"https">>
        ->
            Transport =
                case Scheme of
                    <<"https">> -> tls;
                    <<"http">> -> tcp
                end,
            DefaultPort =
                case Scheme of
                    <<"https">> -> 443;
                    <<"http">> -> 80
                end,
            Port = maps:get(port, Uri, DefaultPort),
            Path0 = maps:get(path, Uri, <<"/">>),
            Path1 =
                case Path0 of
                    <<>> -> <<"/">>;
                    P -> P
                end,
            Path =
                case maps:get(query, Uri, undefined) of
                    undefined -> Path1;
                    Q -> <<Path1/binary, "?", Q/binary>>
                end,
            {ok, #{
                host => binary_to_list(Host),
                port => Port,
                path => Path,
                transport => Transport
            }};
        _ ->
            {error, invalid_url}
    end.

%% @private Build gun connection options, including TLS peer verification for HTTPS.
%%
%% For TLS connections, enables full certificate and hostname verification:
%% - `verify_peer` — reject connections with invalid or untrusted certificates
%% - `cacerts` — use the system CA bundle (OTP 25.1+)
%% - `server_name_indication` — send SNI so virtual hosts present the right cert
%% - `customize_hostname_check` — enforce RFC 6125 hostname matching for HTTPS
-spec build_gun_opts(tcp | tls, string()) -> map().
build_gun_opts(tcp, _Host) ->
    #{transport => tcp};
build_gun_opts(tls, Host) ->
    #{
        transport => tls,
        tls_opts => [
            {verify, verify_peer},
            {cacerts, public_key:cacerts_get()},
            {server_name_indication, Host},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]
    }.

%% @private Convert Beamtalk headers list to gun headers format.
%%
%% Beamtalk headers: list of `[Name, Value]` pairs (binaries).
%% Gun headers: list of `{Name, Value}` tuples.
%% Raises type_error (tagged with `Selector`) for malformed entries.
-spec to_gun_headers(list(), atom()) -> list().
to_gun_headers([], _Selector) ->
    [];
to_gun_headers([[Name, Value] | Rest], Selector) when is_binary(Name), is_binary(Value) ->
    [{Name, Value} | to_gun_headers(Rest, Selector)];
to_gun_headers([{Name, Value} | Rest], Selector) when is_binary(Name), is_binary(Value) ->
    %% Also accept tuple pairs for interop
    [{Name, Value} | to_gun_headers(Rest, Selector)];
to_gun_headers([Bad | _Rest], Selector) ->
    type_error(
        Selector,
        iolist_to_binary([
            <<"Headers must be a List of [Name, Value] binary pairs; got: ">>,
            io_lib:format("~p", [Bad])
        ])
    ).

%% @private Convert gun headers to Beamtalk list-of-lists format.
%%
%% Gun headers: list of `{Name, Value}` tuples.
%% Beamtalk headers: list of `[Name, Value]` pairs.
-spec from_gun_headers(list()) -> list().
from_gun_headers(Headers) ->
    [[Name, Value] || {Name, Value} <- Headers].

%% @private Validate request options, raising type_error on bad values.
-spec validate_request_options(list(), binary(), non_neg_integer()) -> ok.
validate_request_options(Headers, _Body, _Timeout) when not is_list(Headers) ->
    type_error('request:url:options:', <<"Options.headers must be a List">>);
validate_request_options(_Headers, Body, _Timeout) when not is_binary(Body) ->
    type_error('request:url:options:', <<"Options.body must be a String">>);
validate_request_options(_Headers, _Body, Timeout) when
    not is_integer(Timeout) orelse Timeout < 0
->
    type_error(
        'request:url:options:',
        <<"Options.timeout must be a non-negative Integer">>
    );
validate_request_options(_Headers, _Body, _Timeout) ->
    ok.

%% @private Normalise a method value to an uppercase binary.
%%
%% Accepts binary `<<"get">>` or atom `get`/`'GET'`.
%% Raises type_error for any other value.
%% `string:uppercase/1` returns `chardata()`, so we convert to binary explicitly.
-spec normalise_method(term()) -> binary().
normalise_method(Method) when is_binary(Method) ->
    unicode:characters_to_binary(string:uppercase(Method));
normalise_method(Method) when is_atom(Method) ->
    unicode:characters_to_binary(string:uppercase(atom_to_binary(Method, utf8)));
normalise_method(_) ->
    type_error('request:url:options:', <<"Method must be a Symbol or String">>).

%% @private Compute an absolute deadline timestamp in monotonic milliseconds.
%%
%% The deadline is used to enforce a wall-clock timeout across multiple
%% sequential gun:await calls (connect, response headers, body). This
%% prevents a single request from consuming `N * Timeout` milliseconds.
-spec deadline(non_neg_integer()) -> integer().
deadline(Timeout) ->
    erlang:monotonic_time(millisecond) + Timeout.

%% @private Return the remaining milliseconds until the deadline.
%%
%% Returns 0 if the deadline has already passed (gun handles 0 as immediate).
-spec remaining(integer()) -> non_neg_integer().
remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

%% @private Raise a typed HTTP error.
-spec http_error(atom(), map(), binary()) -> no_return().
http_error(Selector, Details, Message) ->
    Error0 = beamtalk_error:new(http_error, 'Http'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, Details),
    Error3 = beamtalk_error:with_hint(Error2, Message),
    beamtalk_error:raise(Error3).

%% @private Raise a type error for a bad argument.
-spec type_error(atom(), binary() | iolist()) -> no_return().
type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'Http'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, iolist_to_binary(Hint)),
    beamtalk_error:raise(Error2).
