%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handler for the `nav-query` operation (BT-2239).

Routes System-Browser navigation queries to the live `SystemNavigation`
facade so editor tooling (the LSP, future LiveView IDE) can delegate
find-references / go-to-implementation / hierarchy queries to the running
image instead of duplicating the AST walk in Rust (the static-first,
live-augmented model of ADR 0024).

This is the runtime side of the BT-2215 epic. Each query is answered by
evaluating the corresponding `SystemNavigation default <query>` expression in
the calling session worker (where these sealed-Object value-type methods run
inline) and serialising the result to a machine-readable JSON shape:

* `implementorsOf` → `{"implementors": [{"class", "meta"}], ...}`
* `sendersOf` / `referencesTo` → `{"sites": [{"class", "selector", "line"}], ...}`

`line` is 1-based and relative to the containing method's source; the LSP
resolves the method definition and adds this offset (see
`beamtalk_core::language_service::method_relative_line_span`).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4]).

%% Export internals for white-box testing of validation and serialisation.
-ifdef(TEST).
-export([build_query/2, is_safe_arg/1, encode_site/1, encode_implementor/1]).
-endif.

-doc "Handle the nav-query op. Returns a JSON binary response.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"nav-query">>, Params, Msg, SessionPid) ->
    Kind = maps:get(<<"kind">>, Params, undefined),
    Arg = maps:get(<<"arg">>, Params, undefined),
    case build_query(Kind, Arg) of
        {error, Err} ->
            beamtalk_repl_json:encode_error(Err, Msg);
        {ok, Expr, ResultShape} ->
            run_query(Expr, ResultShape, Msg, SessionPid)
    end.

%%% Internal

-doc """
Validate the kind/arg pair and build the Beamtalk query expression.

Returns `{ok, Expr, ResultShape}` where `ResultShape` is `sites` or
`implementors`, or `{error, #beamtalk_error{}}` for an unknown kind or an
unsafe argument. `arg` is whitelisted to selector/class-name characters so it
cannot inject arbitrary code into the evaluated expression.
""".
-spec build_query(binary() | undefined, binary() | undefined) ->
    {ok, string(), sites | implementors} | {error, #beamtalk_error{}}.
build_query(_Kind, Arg) when not is_binary(Arg); Arg =:= <<>> ->
    {error, arg_error(<<"nav-query requires a non-empty 'arg'">>)};
build_query(Kind, Arg) ->
    case query_template(Kind) of
        unknown ->
            {error, arg_error(<<"nav-query 'kind' is not a supported navigation query">>)};
        {Prefix, Shape} ->
            case is_safe_arg(Arg) of
                false ->
                    {error, arg_error(<<"nav-query 'arg' must be a selector or class name">>)};
                true ->
                    {ok, Prefix ++ binary_to_list(Arg), Shape}
            end
    end.

-doc """
Map a query kind to its expression prefix and result shape. `implementorsOf`
and `sendersOf` take a selector (prefixed with `#`); `referencesTo` takes a
bareword class name.
""".
-spec query_template(binary() | undefined) -> {string(), sites | implementors} | unknown.
query_template(<<"implementorsOf">>) ->
    {"SystemNavigation default implementorsOf: #", implementors};
query_template(<<"sendersOf">>) ->
    {"SystemNavigation default sendersOf: #", sites};
query_template(<<"referencesTo">>) ->
    {"SystemNavigation default referencesTo: ", sites};
query_template(_) ->
    unknown.

-doc "Evaluate the query expression and encode its result.".
-spec run_query(string(), sites | implementors, beamtalk_repl_protocol:protocol_msg(), pid()) ->
    binary().
run_query(Expr, ResultShape, Msg, SessionPid) ->
    case beamtalk_repl_shell:eval(SessionPid, Expr) of
        {ok, Term, _Output, _Warnings} ->
            encode_result(ResultShape, Term, Msg);
        {error, Reason, _Output, _Warnings} ->
            Structured = beamtalk_repl_errors:ensure_structured_error(Reason),
            beamtalk_repl_json:encode_error(Structured, Msg)
    end.

-spec encode_result(sites | implementors, term(), beamtalk_repl_protocol:protocol_msg()) ->
    binary().
encode_result(sites, Term, Msg) when is_list(Term) ->
    Sites = [encode_site(S) || S <- Term, is_map(S)],
    Base = beamtalk_repl_protocol:base_response(Msg),
    iolist_to_binary(json:encode(Base#{<<"sites">> => Sites, <<"status">> => [<<"done">>]}));
encode_result(implementors, Term, Msg) when is_list(Term) ->
    Impls = [encode_implementor(C) || C <- Term],
    Base = beamtalk_repl_protocol:base_response(Msg),
    Result = Base#{<<"implementors">> => Impls, <<"status">> => [<<"done">>]},
    iolist_to_binary(json:encode(Result));
encode_result(_Shape, _Term, Msg) ->
    %% Defensive: SystemNavigation always returns a List; a non-list result
    %% means the query shape changed. Surface an empty result rather than crash.
    ?LOG_WARNING("nav-query: unexpected non-list result", #{domain => [beamtalk, runtime]}),
    Base = beamtalk_repl_protocol:base_response(Msg),
    iolist_to_binary(json:encode(Base#{<<"sites">> => [], <<"status">> => [<<"done">>]})).

-doc """
Encode one `{#class, #selector, #line}` record. `#class` is a class or
metaclass object whose display name (`"Counter"` / `"Counter class"`) is
produced by the shared `term_to_json` class formatter.
""".
-spec encode_site(map()) -> map().
encode_site(#{class := Class, selector := Selector, line := Line}) ->
    #{
        <<"class">> => beamtalk_repl_json:term_to_json(Class),
        <<"selector">> => to_binary(Selector),
        <<"line">> => Line
    }.

-doc """
Encode one implementor (a class or metaclass object). `meta` is derived from
the `" class"` suffix the formatter appends to metaclass objects, matching the
class-side display name the LSP translation helper expects.
""".
-spec encode_implementor(term()) -> map().
encode_implementor(Class) ->
    Name = beamtalk_repl_json:term_to_json(Class),
    #{<<"class">> => Name, <<"meta">> => has_class_suffix(Name)}.

-spec has_class_suffix(term()) -> boolean().
has_class_suffix(Name) when is_binary(Name) ->
    Size = byte_size(Name) - 6,
    Size >= 0 andalso binary:part(Name, Size, 6) =:= <<" class">>;
has_class_suffix(_) ->
    false.

-spec to_binary(term()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) -> iolist_to_binary(io_lib:format("~tp", [V])).

-doc """
Whitelist the query argument to characters valid in a selector (keyword,
unary, or binary) or a package-qualified class name. Rejects anything that
could break out of the evaluated expression (spaces, quotes, brackets, …).
""".
-spec is_safe_arg(binary()) -> boolean().
is_safe_arg(Arg) ->
    lists:all(fun is_safe_char/1, binary_to_list(Arg)).

-spec is_safe_char(byte()) -> boolean().
is_safe_char(C) ->
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        lists:member(C, "_:+-*/<>=~%&?,@\\|").

-spec arg_error(binary()) -> #beamtalk_error{}.
arg_error(Message) ->
    Err0 = beamtalk_error:new(invalid_argument, 'SystemNavigation'),
    beamtalk_error:with_message(Err0, Message).
