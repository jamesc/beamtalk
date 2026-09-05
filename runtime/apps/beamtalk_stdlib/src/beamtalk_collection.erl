%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime infrastructure for Collection iteration.

BT-505: Provides `to_list/1` — a helper used by compiler-generated
Core Erlang (list_ops.rs) to convert non-list collection receivers
to Erlang lists before passing them to `lists:foldl`.

BT-815: Provides `inject_into/3` — called by the `@primitive "inject:into:"`
body on the abstract Collection class.  Most other collection methods
(collect:, select:, reject:, includes:, detect:, anySatisfy:, allSatisfy:)
are now self-hosted as pure Beamtalk on collection.bt and no longer need
Erlang helpers.

BT-2695: Provides the numeric aggregates `sum/1`, `maximum/1`, `minimum/1`,
and `average/1` — backing the `@primitive` `sum`/`max`/`min`/`average` on
Collection. They fold over `to_list/1`; `max`/`min`/`average` raise
`empty_collection` on an empty collection (BT-3021).
""".

-export([
    average/1,
    from_list_like/2,
    inject_into/3,
    maximum/1,
    minimum/1,
    sum/1,
    to_list/1
]).

%% Explicit `(Erlang beamtalk_collection) raiseEmpty: ... selector: ...` FFI
%% dispatch, not @primitive — see docs/beamtalk-native-erlang.md "The naming
%% rule": the function name is the first keyword with its colon removed, no
%% case conversion, hence camelCase.
-export([raiseEmpty/2, raiseDetectNotFound/1, raiseIndexOutOfBounds/2]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Fold a block over the collection with an accumulator.

Calls Block(Acc, Elem) for each element — accumulator first, element
second — matching the Beamtalk `block value: acc value: each` convention
used by collection.bt's `collect:`, `select:`, and `reject:`.

Hand-rolls the fold rather than wrapping `lists:foldl/3` (which calls
`Fun(Elem, Acc)`): the BT block takes the accumulator first, so a foldl
wrapper would have to swap arguments per element. Beyond avoiding that
wrapper, the loop deliberately threads the accumulator as its *last*
argument (`fold_block(List, Block, Acc)`, not `(List, Acc, Block)`). On the
BEAM JIT (OTP 28) that layout folds ~1.6x faster — close to native
`lists:foldl` — than the accumulator-in-the-middle shape, with no change to
the block call itself (BT-2713). See `runtime/perf/bench_collect_selfhost.escript`.
""".
-spec inject_into(term(), term(), function()) -> term().
inject_into(Self, Initial, Block) ->
    fold_block(to_list(Self), Block, Initial).

%% Tail-recursive fold; accumulator is the last argument (see inject_into/3 doc).
-spec fold_block(list(), function(), term()) -> term().
fold_block([], _Block, Acc) ->
    Acc;
fold_block([Elem | Rest], Block, Acc) ->
    fold_block(Rest, Block, Block(Acc, Elem)).

-doc """
Sum all elements, returning 0 for an empty collection.

Backs `@primitive "sum"` on Collection. Uses native numeric `+` (Integer/Float),
not a dispatched Beamtalk `+` message.
""".
-spec sum(term()) -> number().
sum(Self) ->
    lists:foldl(fun(Elem, Acc) -> Acc + Elem end, 0, to_list(Self)).

-doc """
Return the largest element using Erlang's native term ordering (`>`), not a
dispatched Beamtalk `>` message.

Backs `@primitive "max"` on Collection. Raises `empty_collection` on an empty
collection — there is no sensible maximum of nothing.
""".
-spec maximum(term()) -> term().
maximum(Self) ->
    case to_list(Self) of
        [] -> raise_empty('max');
        List -> lists:max(List)
    end.

-doc """
Return the smallest element using Erlang's native term ordering (`<`), not a
dispatched Beamtalk `<` message.

Backs `@primitive "min"` on Collection. Raises `empty_collection` on an empty
collection.
""".
-spec minimum(term()) -> term().
minimum(Self) ->
    case to_list(Self) of
        [] -> raise_empty('min');
        List -> lists:min(List)
    end.

-doc """
Return the mean of the elements as a float.

Backs `@primitive "average"` on Collection. Raises `empty_collection` on an
empty collection.
""".
-spec average(term()) -> float().
average(Self) ->
    case to_list(Self) of
        [] -> raise_empty('average');
        List -> lists:sum(List) / length(List)
    end.

%% BT-3021: `empty_collection`, not `user_error` — this is the same condition as
%% `#() first`, and callers need to discriminate it from an arbitrary
%% `self error:` raised by user code inside the same `on:do:` block.
-spec raise_empty(atom()) -> no_return().
raise_empty(Selector) ->
    Error0 = beamtalk_error:new(empty_collection, 'Collection'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Hint = iolist_to_binary([
        <<"Cannot compute ">>, atom_to_binary(Selector, utf8), <<" of an empty collection">>
    ]),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, Hint)).

-doc """
Raise `empty_collection` naming `Class` and `Selector`.

BT-3021: pure Beamtalk has no way to raise a *named* error kind — `self error:`
always yields `user_error` — so collection classes written in Beamtalk (e.g.
`Interval`) call this to report empty-collection access with the same kind the
`@primitive` accessors on `List`/`String` raise. This is `empty_collection`-specific
sugar with an auto-generated hint; for any other kind, or a hint specific to
the call site, use `Exception class >> signalKind:class:selector:hint:` (BT-3042)
directly from Beamtalk.
""".
-spec raiseEmpty(atom(), atom()) -> no_return().
raiseEmpty(Class, Selector) ->
    Hint = iolist_to_binary([
        atom_to_binary(Class, utf8),
        <<" is empty; guard with `isEmpty` before calling `">>,
        atom_to_binary(Selector, utf8),
        <<"`">>
    ]),
    beamtalk_error:raise(beamtalk_error:new(empty_collection, Class, Selector, Hint)).

-doc """
Raise `index_out_of_bounds` naming `Class` and `Selector`.

BT-3027: pure Beamtalk has no way to raise a *named* error kind (`self
error:` always yields `user_error`), so collection classes written in
Beamtalk (e.g. `Interval`) call this to report an out-of-range index with
the same kind the `@primitive` accessors on `List`/`Array`/`String` raise.
Unlike `raiseEmpty/2`, this is deliberately not conditioned on emptiness —
callers use it for *every* out-of-range index, empty receiver or not.
""".
-spec raiseIndexOutOfBounds(atom(), atom()) -> no_return().
raiseIndexOutOfBounds(Class, Selector) ->
    Hint = iolist_to_binary([
        <<"Index is out of bounds for ">>,
        atom_to_binary(Class, utf8)
    ]),
    beamtalk_error:raise(beamtalk_error:new(index_out_of_bounds, Class, Selector, Hint)).

-doc """
Raise `not_found` for a `detect:` that ran to completion without matching.

BT-3028: `Collection>>detect:` answers `E`, so there is no in-band way to say
"nothing matched" — it raises, matching the `@primitive` `List>>detect:`. Pure
Beamtalk cannot raise a *named* kind (`self error:` always yields `user_error`),
so the generic implementation calls this. `Class` is the receiver's own class,
so a `Set` reports `Set`, not the abstract `Collection` the method lives on.

The hint deliberately matches `beamtalk_list:detect/2`'s, so a no-match reads
the same whichever collection raised it.
""".
-spec raiseDetectNotFound(atom()) -> no_return().
raiseDetectNotFound(Class) ->
    Hint = <<"No element matched the block; use `detect:ifNone:` to supply a default">>,
    beamtalk_error:raise(beamtalk_error:new(not_found, Class, 'detect:', Hint)).

-doc """
Reconstruct a `collect:`/`select:`/`reject:` result so its type matches the
original receiver, mirroring the pure (non-mutating) list-op path.

The stateful (mutation-threading) list-op codegen folds over `to_list/1` of the
receiver and produces a raw Erlang list. This helper wraps that list back into
the receiver's representation:

- String receiver (binary) → binary (`iolist_to_binary/1`)
- Array receiver → `Array` (`beamtalk_array:from_list/1`)
- anything else (already an Erlang list, or another collection) → the list as-is

Called from compiler-generated Core Erlang (`list_ops`) — see BT-2342.
""".
-spec from_list_like(term(), list()) -> term().
from_list_like(Recv, List) when is_binary(Recv) ->
    erlang:iolist_to_binary(List);
from_list_like(#{'$beamtalk_class' := 'Array'}, List) ->
    beamtalk_array:from_list(List);
from_list_like(_Recv, List) ->
    List.

-doc """
Convert any collection to a list by iterating with do:.

Called from compiler-generated Core Erlang for `do:`, `collect:`,
`select:`, `reject:`, and `inject:into:` when the receiver is not
already an Erlang list.

BT-3022: the accumulator is the *caller's mailbox*, not its process dictionary.
A `Collection` subclass may implement `do:` by delegating to a class-side method,
and class-method dispatch runs the callee in the class gen_server process
(`beamtalk_class_dispatch:class_send/3`). The iteration block is a plain fun, so
it is copied across that boundary and invoked there — a `put/2` inside it would
land in the *class* process's dictionary, and this function would read back its
own untouched accumulator and answer `#()`. Posting each element back to the
process that started the iteration is boundary-agnostic: it works whether the
block runs here, in a class process, or in an actor.

Ordering is preserved by BEAM's pairwise signal-ordering guarantee: the class
process posts every element and then sends its `gen_server:call` reply over the
same sender/receiver pair, so all the elements are already queued ahead of the
reply that releases us.

The `after` clause is load-bearing: on the exception path (a `^` non-local
return or a raise from inside the block) elements may still be queued, and
leaving them behind would hand stray `'$bt_to_list'` messages to whatever
`handle_info/2` owns this process.

Cost, measured on the accumulator in isolation (1000 elements, OTP 28):

    backlog   process dict   mailbox   collector process
          0         56 us     149 us         327 us
       1000         87 us     217 us         355 us
       5000        175 us     387 us         406 us

So the mailbox costs roughly 90ns per element more than the process dictionary
did — real, but a minority of a `to_list/1` call, which also pays a full `do:`
dispatch per element on top. Two things worth knowing before "optimising" this:

- A backlog does *not* cost `O(elements x backlog)`. The naive reading of a
  selective receive says each element re-scans the queued prefix; measurement
  says otherwise (2.6x from a 5000-message backlog, not 5000x), and the process
  dictionary version degrades alongside it, so most of that is heap pressure
  from the queued messages rather than scanning.
- Routing through a dedicated collector process — the obvious "avoid the
  caller's mailbox" answer — is *slower at every backlog measured*: the spawn
  and the cross-process handshake cost more than the receives they replace, and
  the crossover never arrives. Do not switch to it without new numbers.
""".
-spec to_list(term()) -> list().
to_list(Self) when is_list(Self) ->
    Self;
to_list(Self) ->
    Ref = make_ref(),
    Owner = self(),
    try
        Block = fun(Each) ->
            Owner ! {'$bt_to_list', Ref, Each},
            ok
        end,
        beamtalk_primitive:send(Self, 'do:', [Block]),
        collect_posted(Ref, [])
    after
        discard_posted(Ref)
    end.

%% Drain the elements posted by the iteration block, oldest first.
-spec collect_posted(reference(), list()) -> list().
collect_posted(Ref, Acc) ->
    receive
        {'$bt_to_list', Ref, Each} ->
            collect_posted(Ref, [Each | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

%% Drop anything left over after an abnormal exit from the iteration. On the
%% success path `collect_posted/2` has already emptied the queue, so this is a
%% single non-matching mailbox scan.
-spec discard_posted(reference()) -> ok.
discard_posted(Ref) ->
    receive
        {'$bt_to_list', Ref, _Each} ->
            discard_posted(Ref)
    after 0 ->
        ok
    end.
