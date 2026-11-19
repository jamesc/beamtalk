%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_wire_payloads).

-moduledoc """
Test access to the per-op wire payload fixture (BT-5049),
`runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json`.

The fixture holds one example reply per REPL op. The workspace encoder tests
check the encoded output against it; this module lets the tests that build real
rows from live handlers (`list-classes`, `methods`, `inherited-methods`,
`browse-classes`) check that a row a handler produces has the same keys, with
the same JSON types, as the row the fixture pins, so a handler cannot start
sending a new key (or drop one) without the Rust and TypeScript clients seeing
it in the fixture.
""".

-export([fixture/0, response/1, row_shape_mismatches/3, assert_row_shape/3]).

-doc "The decoded fixture.".
-spec fixture() -> map().
fixture() ->
    beamtalk_test_corpus:load_json_fixture([
        "runtime", "apps", "beamtalk_workspace", "test", "fixtures", "repl_op_payloads.json"
    ]).

-doc "The example reply (decoded JSON) the fixture holds under `Id`.".
-spec response(binary()) -> map().
response(Id) ->
    #{<<"payloads">> := Payloads} = fixture(),
    case Payloads of
        #{Id := #{<<"response">> := Response}} -> Response;
        _ -> error({no_such_payload, Id})
    end.

-doc """
How `LiveRow` differs from the first row of the fixture payload `Id` at
`Field` (a list under that key of the payload's reply, e.g. `<<"class_list">>`):
keys missing from or extra in the live row, and keys whose JSON types differ.
A `null` on either side is compatible with any type (a key that is `null` in
one example and a string in another is nullable, not mistyped). `[]` means the
shapes agree.
""".
-spec row_shape_mismatches(binary(), binary(), map()) -> [term()].
row_shape_mismatches(Id, Field, LiveRow) when is_map(LiveRow) ->
    [Expected | _] = maps:get(Field, response(Id)),
    ExpectedKeys = lists:sort(maps:keys(Expected)),
    LiveKeys = lists:sort(maps:keys(LiveRow)),
    [{missing_key, K} || K <- ExpectedKeys -- LiveKeys] ++
        [{unexpected_key, K} || K <- LiveKeys -- ExpectedKeys] ++
        [
            {type_mismatch, K, json_type(maps:get(K, Expected)), json_type(maps:get(K, LiveRow))}
         || K <- ExpectedKeys,
            lists:member(K, LiveKeys),
            not compatible(json_type(maps:get(K, Expected)), json_type(maps:get(K, LiveRow)))
        ].

-doc "Assert `row_shape_mismatches/3` is empty.".
-spec assert_row_shape(binary(), binary(), map()) -> ok.
assert_row_shape(Id, Field, LiveRow) ->
    case row_shape_mismatches(Id, Field, LiveRow) of
        [] -> ok;
        Mismatches -> error({wire_payload_shape_mismatch, Id, Field, Mismatches})
    end.

-spec compatible(atom(), atom()) -> boolean().
compatible(null, _) -> true;
compatible(_, null) -> true;
compatible(T, T) -> true;
compatible(_, _) -> false.

-spec json_type(term()) -> atom().
json_type(null) -> null;
json_type(V) when is_boolean(V) -> boolean;
json_type(V) when is_binary(V) -> string;
json_type(V) when is_number(V) -> number;
json_type(V) when is_list(V) -> list;
json_type(V) when is_map(V) -> map.
