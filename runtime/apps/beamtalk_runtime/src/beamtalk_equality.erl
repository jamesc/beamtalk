%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_equality).

%%% **DDD Context:** Object System Context

-moduledoc """
Overridable value equality (`equals:`) for primitives that scan (BT-2997).

Beamtalk's four equality operators (`=:=`, `=/=`, `==`, `/=`) lower straight to
the Erlang BIFs and never dispatch (ADR 0002), so they cannot be overridden.
`Object>>equals:` is the overridable counterpart. This module is how the
stdlib's *linear scans* honour it from Erlang-implemented primitives.

## Which operations honour `equals:`

Linear scans do — `List>>includes:` here, plus `Collection>>includes:` and
`List>>indexOf:` which are written in Beamtalk and simply send `equals:`.

Keyed containers do **not**, and cannot: `Dictionary` is backed by Erlang maps
(keys compare with `=:=`) and `Set` by `ordsets` (elements compare by term
order, i.e. `==`). Both decide identity inside the VM, below anything the
language can dispatch.

## The reflexivity contract

`equals:` must agree with `=:=` wherever `=:=` holds:

    A =:= B  implies  A equals: B

An override may only make *more* values equal, never fewer — which is what
`Object>>equals:` defaulting to `=:=` already means. Every function here relies
on it: each tries the raw BIF first (`lists:member/2`, C speed) and only falls
back to per-element dispatch when raw equality found nothing. A hit on the fast
path is therefore always a genuine `equals:` hit, and needs no dispatch at all.

An override that answered `false` for `=:=`-identical terms would break that
shortcut. That is a contract violation, not a supported use.

## Dispatch direction

Equality dispatch keys on the left receiver, so the direction matters. These
functions send `equals:` to the **element**, with the searched-for value as the
argument (`each equals: item`) — matching `Collection>>includes:` and the
Smalltalk convention.
""".

-export([eq/2, member/2]).

%% Dispatch targets are resolved at runtime from compiled stdlib modules.
% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({nowarn_function, [eq/2, dispatch_equals/2]}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
True if `Elem equals: Item`, honouring an `equals:` override on `Elem`.

Takes the raw `=:=` fast path first: per the reflexivity contract above, a raw
hit is always an `equals:` hit, so no dispatch is needed. Only a raw miss on an
object receiver dispatches.
""".
-spec eq(term(), term()) -> boolean().
eq(Elem, Item) ->
    Elem =:= Item orelse (beamtalk_primitive:is_object(Elem) andalso dispatch_equals(Elem, Item)).

-doc """
`List>>includes:` — true if any element `equals:` `Item`.

`lists:member/2` runs first and settles every list of primitives, and every
list of objects where a structurally identical element exists, at C speed. The
dispatching scan runs only when that found nothing, so the cost of supporting
overrides falls entirely on the negative case.
""".
-spec member(term(), list()) -> boolean().
member(Item, List) ->
    lists:member(Item, List) orelse equals_scan(List, Item).

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% Scan for an element whose `equals:` override accepts `Item`.
%%
%% `lists:member/2` has already ruled out raw equality, so only object elements
%% can still match — non-objects are skipped without a dispatch.
-spec equals_scan(list(), term()) -> boolean().
equals_scan([Elem | Rest], Item) when
    is_number(Elem);
    is_atom(Elem);
    is_binary(Elem);
    is_bitstring(Elem);
    is_list(Elem);
    is_function(Elem);
    is_reference(Elem);
    is_port(Elem)
->
    %% Local guard for element kinds `is_object/1` can never accept, so a list
    %% of primitives — the overwhelmingly common case, and the one that pays
    %% this scan on every negative `includes:` — skips without a cross-module
    %% call per element.
    %%
    %% Deliberately excludes `is_map` (tagged value-type maps ARE objects),
    %% `is_tuple` (`#beamtalk_object{}` is a record, hence a tuple) and
    %% `is_pid` (actor refs). Those fall through to the real check below.
    equals_scan(Rest, Item);
equals_scan([Elem | Rest], Item) ->
    case beamtalk_primitive:is_object(Elem) andalso dispatch_equals(Elem, Item) of
        true -> true;
        false -> equals_scan(Rest, Item)
    end;
equals_scan([], _Item) ->
    false.

%% Send `equals:` to an object element. Coerces a non-boolean answer from a
%% misbehaving override to `false` rather than letting it leak into a guard.
-spec dispatch_equals(term(), term()) -> boolean().
dispatch_equals(Elem, Item) ->
    beamtalk_message_dispatch:send(Elem, 'equals:', [Item]) =:= true.
