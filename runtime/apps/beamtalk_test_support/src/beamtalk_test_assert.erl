%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_assert).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Runtime half of the structured-error assertion macros in
`include/beamtalk_test_assert.hrl` (BT-4900).

The macros that check message / hint text cannot be written as a pure
pattern match, so they call `contains/2` here.
""".

-export([contains/2]).

-doc """
True when `Needle` occurs in `Text`.

`Text` is a `{beamtalk_string, Binary}`, a binary, or a charlist - the
shapes `#beamtalk_error{}` message and hint fields take. `undefined` (an
unset hint) never contains anything.
""".
-spec contains(term(), binary()) -> boolean().
contains({beamtalk_string, Bin}, Needle) when is_binary(Bin) ->
    contains(Bin, Needle);
contains(Bin, Needle) when is_binary(Bin) ->
    binary:match(Bin, Needle) =/= nomatch;
contains(List, Needle) when is_list(List) ->
    contains(unicode:characters_to_binary(List), Needle);
contains(_Other, _Needle) ->
    false.
