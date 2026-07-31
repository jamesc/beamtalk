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

Linear scans do. Through this module: `List>>includes:`, `Array>>includes:`,
and `Dictionary>>includes:` (which searches *values*, not keys). Directly, by
sending `equals:` from Beamtalk: `Collection>>includes:`, `List>>indexOf:`,
`TestCase>>assert:equals:`.

Keying and deduplication do **not**, and cannot: `Dictionary` *keys* are Erlang
map keys and `Set` elements live in a term-order-sorted list, both decided
inside the VM; `List>>unique` needs an order. All three use raw `=:=`. This is
the same constraint Java's `equals`/`hashCode` contract expresses — membership
in a keyed structure needs an order or a hash that a user-defined `equals:`
cannot supply.

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

## Actors are never dispatched to

Only tagged value-type maps are asked. Actor references — bare pids and
`#beamtalk_object{}` — answer by process identity via raw `=:=`, because
sending them `equals:` would be a synchronous call into a live process from
inside a membership test. See `dispatchable/1`.

## Dispatch direction

Equality dispatch keys on the left receiver, so the direction matters. These
functions send `equals:` to the **element**, with the searched-for value as the
argument (`each equals: item`) — matching `Collection>>includes:` and the
Smalltalk convention.
""".

-export([eq/2, member/2, scan/2]).

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
    Elem =:= Item orelse (dispatchable(Elem) andalso dispatch_equals(Elem, Item)).

-doc """
`List>>includes:` — true if any element `equals:` `Item`.

`lists:member/2` runs first and settles every list of primitives, and every
list of objects where a structurally identical element exists, at C speed. The
dispatching scan runs only when that found nothing, so the cost of supporting
overrides falls entirely on the negative case.
""".
-spec member(term(), list()) -> boolean().
member(Item, List) ->
    lists:member(Item, List) orelse scan(List, Item).

-doc """
Scan for an element whose `equals:` override accepts `Item`, assuming raw
equality has already been ruled out.

Exported so callers that can test raw equality more cheaply than
`lists:member/2` — `beamtalk_array`, which folds over its backing map without
materialising a list — can run their own fast path first and fall back here
only on a miss. Calling this *without* having ruled out raw equality first will
miss elements that are `=:=` but not dispatchable.
""".
-spec scan(list(), term()) -> boolean().
scan([Elem | Rest], Item) when not is_map(Elem) ->
    %% Fast reject in the clause head. `dispatchable/1` accepts only tagged
    %% maps, so anything that is not a map is skipped without even a local
    %% call — and a list of primitives, the case that pays this scan on every
    %% negative `includes:`, is exactly that.
    scan(Rest, Item);
scan([Elem | Rest], Item) ->
    case is_map_key('$beamtalk_class', Elem) andalso dispatch_equals(Elem, Item) of
        true -> true;
        false -> scan(Rest, Item)
    end;
scan([], _Item) ->
    false.

%%% ============================================================================
%%% Internal
%%% ============================================================================

-doc """
Whether sending `equals:` to `X` is meaningful and safe.

True only for tagged value-type / tagged-collection maps. Deliberately narrower
than `beamtalk_primitive:is_object/1`, which also accepts **actor references**
— bare pids and `#beamtalk_object{}` records.

Actors are excluded because actor equality *is* process identity: two spawns of
the same class are never equal, which raw `=:=` already answers correctly.
Dispatching to one would instead make a synchronous `gen_server` call per actor
element, turning a side-effect-free membership test into something that blocks
on live processes and can raise `deadlock_detected` (BT-1325 cycle detection)
or time out — from `aList includes: anActor`, which could not fail before.
Cheaper and safer to answer by identity.

Being a plain local guard also means the common non-object element costs no
cross-module call at all.
""".
-spec dispatchable(term()) -> boolean().
dispatchable(X) when is_map(X) ->
    is_map_key('$beamtalk_class', X);
dispatchable(_NotATaggedMap) ->
    false.

%% Send `equals:` to an object element. Coerces a non-boolean answer from a
%% misbehaving override to `false` rather than letting it leak into a guard.
-spec dispatch_equals(term(), term()) -> boolean().
dispatch_equals(Elem, Item) ->
    beamtalk_message_dispatch:send(Elem, 'equals:', [Item]) =:= true.
