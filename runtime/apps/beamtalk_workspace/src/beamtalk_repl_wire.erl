%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_wire).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The wire projection shared by the `{value, Map}` ops (BT-5204, BT-5205).

An op that answers `{value, Map}` answers a term a LiveView client reads as
is and a WebSocket client reads as JSON. `project/1` makes that term the JSON
shape: binary keys, `null` for an absent value, atoms as their names. The
class tag is internal and is dropped.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([project/1]).

-doc """
Project a runtime term to the wire shape: map keys and atoms become binaries,
`nil` / `undefined` become `null`, booleans, numbers and binaries pass
through, lists recurse. Anything else (a pid, a tuple) is shown as its `~p`
text so a reply never crashes the encoder.
""".
-spec project(term()) -> term().
project(Map) when is_map(Map) ->
    maps:from_list([
        {key(K), project(V)}
     || {K, V} <- maps:to_list(Map), K =/= ?BT_CLASS_TAG_KEY
    ]);
project(List) when is_list(List) -> [project(E) || E <- List];
project(Bool) when is_boolean(Bool) -> Bool;
project(nil) ->
    null;
project(undefined) ->
    null;
project(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
project(Bin) when is_binary(Bin) -> Bin;
project(N) when is_number(N) -> N;
project(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

-spec key(term()) -> binary().
key(K) when is_atom(K) -> atom_to_binary(K, utf8);
key(K) when is_binary(K) -> K;
key(K) -> iolist_to_binary(io_lib:format("~p", [K])).
