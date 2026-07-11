%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json).

%%% **DDD Context:** Object System Context

-moduledoc """
JSON class implementation — JSON encoding/decoding via OTP json module.

JSON provides class-side methods for parsing and generating JSON strings.
Wraps the OTP `json` module (OTP 27+) with proper type mapping and structured error handling.

## Type Mapping

| JSON          | Beamtalk     |
|---------------|--------------|
| object        | Dictionary   |
| array         | List         |
| string        | String       |
| number (int)  | Integer      |
| number (float)| Float        |
| true/false    | true/false   |
| null          | nil          |

Custom objects can opt into JSON generation by implementing an `asJson`
instance method returning a JSON-representable value (BT-2818).

## Methods

| Selector        | Description                              |
|-----------------|------------------------------------------|
| `parse:`        | JSON string → Beamtalk value             |
| `generate:`     | Beamtalk value → JSON string             |
| `prettyPrint:`  | Beamtalk value → formatted JSON string   |
""".

-export(['parse:'/1, 'generate:'/1, 'prettyPrint:'/1]).
-export([parse/1, generate/1, prettyPrint/1]).
-export([prettify_term/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Parse a JSON string into a Beamtalk value.

JSON objects become Dictionaries (maps with binary keys), arrays become
Lists, strings become Strings, numbers become Integer or Float,
true/false stay as atoms, null becomes nil.

Returns `Result ok: value` on success, `Result error:` on invalid JSON.
Type error (non-String argument) still raises.
""".
-spec 'parse:'(binary()) -> beamtalk_result:t().
'parse:'(JsonStr) when is_binary(JsonStr) ->
    try
        Value = json:decode(JsonStr),
        beamtalk_result:from_tagged_tuple({ok, normalize_decoded(Value)})
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        error:Reason when is_tuple(Reason); Reason =:= unexpected_end ->
            Error2 = beamtalk_error:new(
                parse_error, 'Json', 'parse:', <<"Check that the string is valid JSON">>
            ),
            beamtalk_result:from_tagged_tuple({error, Error2});
        _:Reason ->
            Error = beamtalk_error:with_details(
                beamtalk_error:new(
                    parse_error, 'Json', 'parse:', <<"Check that the string is valid JSON">>
                ),
                #{reason => Reason}
            ),
            beamtalk_result:from_tagged_tuple({error, Error})
    end;
'parse:'(_) ->
    beamtalk_error:raise_type_error('Json', 'parse:', <<"Argument must be a String">>).

-doc """
Generate a JSON string from a Beamtalk value.

Dictionaries become JSON objects, Lists become arrays, Strings become
JSON strings, Integer/Float become numbers, true/false become JSON
booleans, nil becomes null. Custom objects that implement `asJson`
are converted via that hook (BT-2818).
""".
-spec 'generate:'(term()) -> binary().
'generate:'(Value) ->
    try
        Prepared = prepare_for_encode(Value),
        iolist_to_binary(json:encode(Prepared))
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        error:{unsupported_type, _} ->
            beamtalk_error:raise_type_error(
                'Json', 'generate:', <<"Value cannot be converted to JSON">>
            );
        _:Reason ->
            Error = beamtalk_error:with_details(
                beamtalk_error:new(
                    type_error, 'Json', 'generate:', <<"Value cannot be converted to JSON">>
                ),
                #{reason => Reason}
            ),
            beamtalk_error:raise(Error)
    end.

-doc "Generate a pretty-printed JSON string with indentation.".
-spec 'prettyPrint:'(term()) -> binary().
'prettyPrint:'(Value) ->
    try
        Prepared = prepare_for_encode(Value),
        Compact = iolist_to_binary(json:encode(Prepared)),
        prettify(Compact)
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        error:{unsupported_type, _} ->
            beamtalk_error:raise_type_error(
                'Json', 'prettyPrint:', <<"Value cannot be converted to JSON">>
            );
        _:Reason ->
            Error = beamtalk_error:with_details(
                beamtalk_error:new(
                    type_error, 'Json', 'prettyPrint:', <<"Value cannot be converted to JSON">>
                ),
                #{reason => Reason}
            ),
            beamtalk_error:raise(Error)
    end.

%%% ============================================================================
%%% FFI aliases — no-colon names for Erlang FFI dispatch
%%% ============================================================================

-doc "FFI alias for parse:/1 — called via (Erlang beamtalk_json) parse: str.".
-spec parse(binary()) -> term().
parse(X) -> 'parse:'(X).

-doc "FFI alias for generate:/1 — called via (Erlang beamtalk_json) generate: val.".
-spec generate(term()) -> binary().
generate(X) -> 'generate:'(X).

-doc """
FFI alias for prettyPrint:/1 — called via (Erlang beamtalk_json) prettyPrint: val.
""".
-spec prettyPrint(term()) -> binary().
prettyPrint(X) -> 'prettyPrint:'(X).

-doc """
Encode a term to pretty-printed JSON binary.
Used by runtime internals (trace_store, workspace_meta) for human-readable output.
""".
-spec prettify_term(term()) -> binary().
prettify_term(Term) ->
    Compact = iolist_to_binary(json:encode(Term)),
    prettify(Compact).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc """
Normalize JSON decoded values to Beamtalk conventions.

json:decode returns `null` as the atom `null`; Beamtalk uses `nil`.
""".
-spec normalize_decoded(term()) -> term().
normalize_decoded(null) ->
    nil;
normalize_decoded(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> normalize_decoded(V) end, Map);
normalize_decoded(List) when is_list(List) ->
    [normalize_decoded(E) || E <- List];
normalize_decoded(Other) ->
    Other.

-doc """
Prepare a Beamtalk value for JSON encoding.

Beamtalk uses `nil` for null; json:encode expects `null`.
Maps with `$beamtalk_class` tags are stripped of metadata.

Custom objects (Value instances, actors, ...) that implement `asJson`
are converted via that hook (BT-2818): the hook's return value is
prepared recursively, so it may itself contain further `asJson` objects.
Tagged maps whose class does not implement `asJson` keep the legacy
behaviour of encoding their user fields directly as a JSON object.
""".
-spec prepare_for_encode(term()) -> term().
prepare_for_encode(nil) ->
    null;
prepare_for_encode(Map) when is_map(Map) ->
    case beamtalk_tagged_map:class_of(Map) of
        undefined ->
            encode_map_fields(Map);
        'Dictionary' ->
            encode_map_fields(Map);
        _Class ->
            case try_as_json_hook(Map) of
                {ok, Prepared} -> Prepared;
                no_hook -> encode_map_fields(Map)
            end
    end;
prepare_for_encode(List) when is_list(List) ->
    [prepare_for_encode(E) || E <- List];
prepare_for_encode(true) ->
    true;
prepare_for_encode(false) ->
    false;
prepare_for_encode(V) when is_integer(V) -> V;
prepare_for_encode(V) when is_float(V) -> V;
prepare_for_encode(V) when is_binary(V) -> V;
prepare_for_encode(V) when is_atom(V) ->
    %% Convert atoms (symbols) to strings for JSON compatibility
    atom_to_binary(V, utf8);
prepare_for_encode(Other) ->
    case try_as_json_hook(Other) of
        {ok, Prepared} ->
            Prepared;
        no_hook ->
            %% No selector — callers add the correct one via their catch blocks
            Error0 = beamtalk_error:new(type_error, 'Json'),
            Error1 = beamtalk_error:with_details(Error0, #{value => Other}),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<
                    "Only Dictionary, List, String, Integer, Float, Boolean, and nil "
                    "convert to JSON natively; implement asJson to give this class "
                    "a JSON representation"
                >>
            ),
            beamtalk_error:raise(Error2)
    end.

-doc """
Encode a map's entries as a JSON object, stripping the `$beamtalk_class` tag.
""".
-spec encode_map_fields(map()) -> map().
encode_map_fields(Map) ->
    Cleaned = maps:remove('$beamtalk_class', Map),
    maps:map(fun(_K, V) -> prepare_for_encode(V) end, Cleaned).

-doc """
Dispatch the `asJson` conversion hook on a custom object (BT-2818).

Returns `{ok, Prepared}` when the object's class implements `asJson`,
`no_hook` otherwise. A hook that returns the receiver unchanged raises a
type error rather than recursing forever.
""".
-spec try_as_json_hook(term()) -> {ok, term()} | no_hook.
try_as_json_hook(Value) ->
    case beamtalk_primitive:responds_to(Value, 'asJson') of
        true ->
            Json = beamtalk_primitive:send(Value, 'asJson', []),
            case Json =:= Value of
                true ->
                    Error0 = beamtalk_error:new(type_error, 'Json'),
                    Error1 = beamtalk_error:with_details(Error0, #{value => Value}),
                    Error2 = beamtalk_error:with_hint(
                        Error1,
                        <<"asJson must return a JSON-representable value, not self">>
                    ),
                    beamtalk_error:raise(Error2);
                false ->
                    {ok, prepare_for_encode(Json)}
            end;
        false ->
            no_hook
    end.

-doc """
Pretty-print a compact JSON binary with 2-space indentation.
Custom JSON pretty-printer (does not require json:format/1 from OTP 27.1+).
""".
-spec prettify(binary()) -> binary().
prettify(Bin) ->
    prettify(Bin, 0, false, []).

-spec prettify(binary(), non_neg_integer(), boolean(), iolist()) -> binary().
prettify(<<>>, _Depth, _InStr, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
prettify(<<$\\, C, Rest/binary>>, Depth, true, Acc) ->
    prettify(Rest, Depth, true, [C, $\\ | Acc]);
prettify(<<$", Rest/binary>>, Depth, InStr, Acc) ->
    prettify(Rest, Depth, not InStr, [$" | Acc]);
prettify(<<C, Rest/binary>>, Depth, true, Acc) ->
    prettify(Rest, Depth, true, [C | Acc]);
prettify(<<${, Rest/binary>>, Depth, false, Acc) ->
    NewDepth = Depth + 1,
    case Rest of
        <<$}, _/binary>> ->
            prettify(Rest, NewDepth, false, [${ | Acc]);
        _ ->
            prettify(Rest, NewDepth, false, [indent(NewDepth), $\n, ${ | Acc])
    end;
prettify(<<$[, Rest/binary>>, Depth, false, Acc) ->
    NewDepth = Depth + 1,
    case Rest of
        <<$], _/binary>> ->
            prettify(Rest, NewDepth, false, [$[ | Acc]);
        _ ->
            prettify(Rest, NewDepth, false, [indent(NewDepth), $\n, $[ | Acc])
    end;
prettify(<<$}, Rest/binary>>, Depth, false, Acc) ->
    NewDepth = max(0, Depth - 1),
    prettify(Rest, NewDepth, false, [$}, indent(NewDepth), $\n | Acc]);
prettify(<<$], Rest/binary>>, Depth, false, Acc) ->
    NewDepth = max(0, Depth - 1),
    prettify(Rest, NewDepth, false, [$], indent(NewDepth), $\n | Acc]);
prettify(<<$,, Rest/binary>>, Depth, false, Acc) ->
    prettify(Rest, Depth, false, [indent(Depth), $\n, $, | Acc]);
prettify(<<$:, Rest/binary>>, Depth, false, Acc) ->
    prettify(Rest, Depth, false, [$\s, $: | Acc]);
prettify(<<$\s, Rest/binary>>, Depth, false, Acc) ->
    prettify(Rest, Depth, false, Acc);
prettify(<<C, Rest/binary>>, Depth, false, Acc) ->
    prettify(Rest, Depth, false, [C | Acc]).

-spec indent(non_neg_integer()) -> binary().
indent(0) -> <<>>;
indent(N) -> binary:copy(<<"  ">>, N).
