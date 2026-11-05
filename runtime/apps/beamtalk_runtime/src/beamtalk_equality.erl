%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_equality).

%%% **DDD Context:** Object System Context

-moduledoc """
Overridable value equality (`equals:`) for primitives that scan.

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

-export([eq/2, member/2, scan/2, primitive_equals/2]).

%% Dispatch targets are resolved at runtime from compiled stdlib modules.
% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({nowarn_function, [eq/2, dispatch_equals/2, primitive_equals/2]}).

-include("beamtalk.hrl").

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

-doc """
An `equals:` *send* to a primitive receiver (BT-4416).

`beamtalk_message_dispatch:send/3` routes `Receiver equals: Arg` here when
`Receiver` is a primitive value (`Integer`, `Float`, `Binary`, `String`,
`Symbol`, `Boolean`, `nil`, `List`, `Block`, `Bitstring`). The full dynamic
send walks the receiver's dispatch chain — `Binary` → `Collection` → `Value`
→ `Object` — with an extension-registry probe at every level, ~0.8µs per
call, to arrive at `Object>>equals:`, which is `=:=`.

Two shortcuts, both preserving the send's answer:

1. **Raw hit.** By the reflexivity contract above, `X =:= Y` implies
   `X equals: Y`, so a raw hit answers `true` with no dispatch at all.
2. **Raw miss, default `equals:`.** If no class on the receiver's chain
   overrides `equals:` — no compiled method below `Object` and no extension
   at any level — the send would run `Object>>equals:`, i.e. `=:=`, which
   just answered `false`. Whether the chain is default is memoised per class
   in `beamtalk_class_derived_cache`, which every extension `register`/
   `unregister` and every class write flushes, so an override installed later
   is seen by the next send.

Anything else — an override exists, or the chain is not yet published —
takes the ordinary primitive send.
""".
-spec primitive_equals(term(), term()) -> term().
primitive_equals(X, Y) when X =:= Y ->
    true;
primitive_equals(X, Y) ->
    case default_equals(beamtalk_primitive:class_of(X)) of
        true -> false;
        false -> beamtalk_primitive:send(X, 'equals:', [Y])
    end.

%%% ============================================================================
%%% Internal
%%% ============================================================================

-doc """
True if an instance of `Class` runs `Object>>equals:` — nothing on its
superclass chain below `Object` defines `equals:` or has it as an extension.

Mirrors the order a compiled primitive `dispatch/3` resolves a selector in:
at each class, its own methods first, then its extensions, then the
superclass. The answer is memoised in `beamtalk_class_derived_cache`, and a
`true` answer additionally in the calling process's dictionary, tagged with
the cache generation read before computing it: a hot `equals:` loop then
costs a generation read and a `get/1` rather than an ETS read, and any class
write or extension (un)registration bumps the generation, which turns every
per-process memo into a miss. Until every class on the chain has published
its method records (early bootstrap, an unregistered class) the answer is
`false`, which sends.
""".
-spec default_equals(atom()) -> boolean().
default_equals(Class) ->
    Gen = beamtalk_class_derived_cache:generation(),
    MemoKey = {?MODULE, default_equals, Class},
    case get(MemoKey) of
        Gen when Gen =/= undefined ->
            true;
        _ ->
            Default = beamtalk_class_derived_cache:fetch({default_equals, Class}, fun() ->
                case equals_owner(Class) of
                    unpublished -> {nocache, false};
                    Owner -> {cache, Owner =:= 'Object'}
                end
            end),
            case Default andalso Gen =/= undefined of
                true -> put(MemoKey, Gen);
                false -> ok
            end,
            Default
    end.

%% The class whose `equals:` an instance of `Class` runs, `{extension, C}` for
%% an extension on `C`, `none` if the chain defines none, or `unpublished`.
-spec equals_owner(atom()) -> atom() | {extension, atom()} | unpublished.
equals_owner(Class) ->
    StepFun = fun(Name, _Depth) ->
        case beamtalk_class_metadata:lookup_method_record(Name, instance, 'equals:') of
            {ok, _} ->
                {found, Name};
            not_found ->
                case beamtalk_extensions:has(Name, 'equals:') of
                    true ->
                        {found, {extension, Name}};
                    false ->
                        case
                            {
                                beamtalk_class_metadata:lookup_instance_methods(Name),
                                beamtalk_class_metadata:lookup_superclass(Name)
                            }
                        of
                            {{ok, _}, {ok, Root}} when Root =:= none; Root =:= nil -> not_found;
                            {{ok, _}, {ok, Super}} -> {next, Super};
                            _ -> {found, unpublished}
                        end
                end
        end
    end,
    try beamtalk_hierarchy:walk_ancestors(Class, StepFun, ?MAX_HIERARCHY_DEPTH + 1) of
        {found, Owner} -> Owner;
        not_found -> none;
        {max_depth_exceeded, _} -> unpublished
    catch
        %% Extension or metadata table not created yet (early bootstrap).
        error:badarg -> unpublished
    end.

-doc """
Whether sending `equals:` to `X` is meaningful and safe.

True only for tagged value-type / tagged-collection maps. Deliberately narrower
than `beamtalk_primitive:is_object/1`, which also accepts **actor references**
— bare pids and `#beamtalk_object{}` records.

Actors are excluded because actor equality *is* process identity: two spawns of
the same class are never equal, which raw `=:=` already answers correctly.
Dispatching to one would instead make a synchronous `gen_server` call per actor
element, turning a side-effect-free membership test into something that blocks
on live processes and can raise `deadlock_detected` (cycle detection)
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
