%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interval).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper for `Interval` materialisation (ADR 0095 §6).

`Interval` is a lazy arithmetic sequence (`from`/`to`/`step`); `asList` realises
it into a concrete Beamtalk `List` (a plain Erlang list). This backs the canonical
large-collection example `(1 to: 100000) asList inspect`.
""".

-export([from/3]).

-doc """
Materialise the arithmetic sequence `From`, `From + Step`, … bounded by `To`
(inclusive) into a Beamtalk `List`.

Returns `[]` for a zero step or a step whose direction does not move `From` toward
`To` (matching `Interval >> size`'s empty cases), so it never loops forever or
raises on a degenerate interval.
""".
-spec from(integer(), integer(), integer()) -> [integer()].
from(From, To, Step) when is_integer(From), is_integer(To), is_integer(Step), Step =/= 0 ->
    case Step > 0 of
        true when From =< To -> lists:seq(From, To, Step);
        false when From >= To -> lists:seq(From, To, Step);
        _ -> []
    end;
from(_From, _To, _Step) ->
    [].
