%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime infrastructure for Collection iteration.

Provides `to_list/1` — a helper used by compiler-generated
Core Erlang (list_ops.rs) to convert non-list collection receivers
to Erlang lists before passing them to `lists:foldl`.

Provides `inject_into/3` — called by the `@primitive "inject:into:"`
body on the abstract Collection class. Most other collection methods
(collect:, select:, reject:, includes:, detect:, anySatisfy:, allSatisfy:)
are self-hosted as pure Beamtalk on collection.bt and no longer need
Erlang helpers.

Provides the numeric aggregates `sum/1`, `maximum/1`, `minimum/1`,
and `average/1` — backing the `@primitive` `sum`/`max`/`min`/`average` on
Collection. They fold over `to_list/1`; `max`/`min`/`average` raise
`empty_collection` on an empty collection.
""".

-include_lib("beamtalk_runtime/include/beamtalk_sentinel_tags.hrl").

-export([
    average/1,
    from_list_like/2,
    inject_into/3,
    maximum/1,
    minimum/1,
    sum/1,
    to_list/1,
    to_list_for_threading/2,
    guard_writing_fold/2,
    keyed_foldl/5
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
the block call itself. See `runtime/perf/bench_collect_selfhost.escript`.
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
Return the largest element.

Backs `@primitive "max"` on Collection. Raises `empty_collection` on an empty
collection — there is no sensible maximum of nothing.

When every element is a number, or every element is a String, or every
element is a Symbol, this is `lists:max/1` (native order; for Strings and
Symbols that is today's behaviour). Any other mix is folded pairwise with a
dispatched `>`, exactly as `Number>>max:` does (`Acc > Elem` keeps `Acc`,
otherwise `Elem`), so a user type's `>` and the `greaterThanFromNumber:`
hooks are honoured and `#(3, nil) max` raises what `3 max: nil` raises.
""".
-spec maximum(term()) -> term().
maximum(Self) ->
    case to_list(Self) of
        [] ->
            raise_empty('max');
        [First | _] = List when is_number(First) ->
            %% Every non-number sorts above every number in term order, so a
            %% number answer proves the whole list is numbers (BT-5266).
            case lists:max(List) of
                Max when is_number(Max) -> Max;
                _ -> extreme(List, '>')
            end;
        List ->
            extreme(List, '>')
    end.

-doc """
Return the smallest element.

Backs `@primitive "min"` on Collection. Raises `empty_collection` on an empty
collection. Same strategy as `maximum/1`, with a dispatched `<`
(`Acc < Elem` keeps `Acc`, otherwise `Elem`, as `Number>>min:`). On the
dispatched path a tie takes the later element, as the pairwise fold does.
""".
-spec minimum(term()) -> term().
minimum(Self) ->
    case to_list(Self) of
        [] ->
            raise_empty('min');
        [First | Rest] = List when is_number(First) ->
            case num_min(Rest, First) of
                {ok, Min} -> Min;
                not_numbers -> extreme(List, '<')
            end;
        List ->
            extreme(List, '<')
    end.

%% All-number fast path for `min`: one pass that tracks the minimum and gives
%% up at the first non-number. Non-numbers sort above numbers, so unlike `max`
%% `lists:min/1` cannot prove the list is all numbers. The comparison comes
%% before `is_number/1` in each guard, so the check rides along with the one
%% compare per element. First of equal elements wins, as in `lists:min/1`.
-spec num_min(list(), number()) -> {ok, number()} | not_numbers.
num_min([H | T], Acc) when H < Acc, is_number(H) -> num_min(T, H);
num_min([H | T], Acc) when H >= Acc, is_number(H) -> num_min(T, Acc);
num_min([], Acc) -> {ok, Acc};
num_min(_, _) -> not_numbers.

%% Strings (all binaries) and Symbols (all atoms) keep native term order;
%% any other mix is folded pairwise with the dispatched operator.
-spec extreme([term(), ...], '>' | '<') -> term().
extreme([First | Rest] = List, Op) ->
    case native_orderable(First, Rest) of
        true when Op =:= '>' -> lists:max(List);
        true -> lists:min(List);
        false -> fold_dispatched(Rest, First, Op)
    end.

-spec native_orderable(term(), list()) -> boolean().
native_orderable({beamtalk_string, _}, Rest) ->
    lists:all(fun is_string/1, Rest);
native_orderable(First, Rest) ->
    is_symbol(First) andalso lists:all(fun is_symbol/1, Rest).

%% A String is the tagged tuple `{beamtalk_string, Bin}` (BT-4170); a Symbol
%% is an atom other than the ones that stand for nil and the Booleans.
-spec is_string(term()) -> boolean().
is_string({beamtalk_string, _}) -> true;
is_string(_) -> false.

-spec is_symbol(term()) -> boolean().
is_symbol(nil) -> false;
is_symbol(true) -> false;
is_symbol(false) -> false;
is_symbol(A) -> is_atom(A).

-spec fold_dispatched(list(), term(), '>' | '<') -> term().
fold_dispatched([], Acc, _Op) ->
    Acc;
fold_dispatched([Elem | Rest], Acc, Op) ->
    case beamtalk_message_dispatch:send(Acc, Op, [Elem]) of
        true ->
            fold_dispatched(Rest, Acc, Op);
        false ->
            fold_dispatched(Rest, Elem, Op);
        Other ->
            %% `Number>>max:` is `(self > other) ifTrue: [self] ifFalse: [other]`,
            %% so a non-Boolean answer is *sent* `ifTrue:ifFalse:` (DNU unless
            %% it understands it). Do exactly that, for the same error.
            Chosen = beamtalk_message_dispatch:send(Other, 'ifTrue:ifFalse:', [
                fun() -> Acc end, fun() -> Elem end
            ]),
            fold_dispatched(Rest, Chosen, Op)
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

%% `empty_collection`, not `user_error` — this is the same condition as
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

Pure Beamtalk has no way to raise a *named* error kind — `self error:`
always yields `user_error` — so collection classes written in Beamtalk (e.g.
`Interval`) call this to report empty-collection access with the same kind the
`@primitive` accessors on `List`/`String` raise. This is `empty_collection`-specific
sugar with an auto-generated hint; for any other kind, or a hint specific to
the call site, use `Exception class >> signalKind:class:selector:hint:`
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

Pure Beamtalk has no way to raise a *named* error kind (`self
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

`Collection>>detect:` answers `E`, so there is no in-band way to say
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

- String receiver (tagged) → `String`: elements unwrap, join, wrap
- Binary receiver (bare binary) → `Binary` (`iolist_to_binary/1`)
- Array receiver → `Array` (`beamtalk_array:from_list/1`)
- Set receiver → `Set` (`beamtalk_set:from_list/1`, deduplicating) — the
  species `Set>>collect:`/`select:` answer on the pure path (BT-3615)
- raw-map receiver (a `Dictionary`) → a map (`maps:from_list/1`); the list must
  be `{K, V}` pairs, as a keyed fold answers (BT-5097). A `.beam` compiled
  before BT-5097 answers bare elements here, so running it on this runtime
  raises `badarg`
- a `Tuple` receiver (per `beamtalk_primitive:class_of/1`) → a tuple (`list_to_tuple/1`; BT-5167)
- anything else (already an Erlang list, or another collection) → the list as-is

Called from compiler-generated Core Erlang (`list_ops`).
""".
-spec from_list_like(term(), list()) -> term().
from_list_like({beamtalk_string, _}, List) ->
    %% BT-4170: a stateful `collect:`/`select:` over a `String` folds
    %% grapheme `String`s; unwrap each before joining, then wrap the
    %% result — mirroring the pure path (`String withAll:`).
    beamtalk_primitive:string_from_binary(
        erlang:iolist_to_binary([beamtalk_primitive:string_to_binary(E) || E <- List])
    );
from_list_like(Recv, List) when is_tuple(Recv) ->
    case beamtalk_primitive:class_of(Recv) of
        %% BT-5167: a writing `collect:`/`select:`/`reject:` over a `Tuple`
        %% answers a `Tuple`, as the pure path does (`Tuple withAll:`).
        'Tuple' -> list_to_tuple(List);
        _ -> List
    end;
from_list_like(Recv, List) when is_binary(Recv) ->
    %% BT-4170: elements may be byte integers or `String`s (a `collect:`
    %% block answering text); unwrap the tags, then assemble bytes —
    %% mirroring the pure path (`Binary withAll:`).
    erlang:iolist_to_binary(beamtalk_primitive:deep_unwrap_strings(List));
from_list_like(#{?BT_CLASS_TAG_KEY := 'Array'}, List) ->
    beamtalk_array:from_list(List);
from_list_like(#{?BT_CLASS_TAG_KEY := 'Set'}, List) ->
    beamtalk_set:from_list(List);
from_list_like(Recv, Pairs) when is_map(Recv) ->
    case beamtalk_primitive:is_object(Recv) of
        %% BT-5097: a keyed fold answers `{Key, Item}` pairs.
        false -> maps:from_list(Pairs);
        true -> Pairs
    end;
from_list_like(_Recv, List) ->
    List.

-doc """
The fold call of a writing `collect:`/`select:`/`reject:` over a receiver that is
not an Erlang list (BT-5097). A `Dictionary` is folded over its `{K, V}` pairs
with the compiled lambda (after the same receiver guard as any other non-list
receiver, `guard_writing_fold/2`); after each call the one element the lambda added to
the result list is recorded under its key, so the epilogue's `from_list_like/2`
rebuilds a `Dictionary`. Any other receiver folds over `to_list_for_threading/2`,
as before.

`ListPath` is the element positions from the accumulator to the result list.
It and the "adds at most one element per call" this relies on are not decided
here: codegen builds the call from the fold kind's declared result list and
checks both (`KeyedFoldCall`, `VerifyError::KeyedFoldPathMismatch`).
""".
-spec keyed_foldl(fun((term(), term()) -> term()), term(), term(), atom(), [pos_integer()]) ->
    term().
keyed_foldl(Lambda, Init, Recv, Selector, ListPath) ->
    case is_map(Recv) andalso not beamtalk_primitive:is_object(Recv) of
        true ->
            %% The ADR 0133 receiver guard, once per call: a class's own override of
            %% the selector (or of a message it runs through) wins dispatch, so the
            %% in-place fold would skip it.
            ok = guard_writing_fold(Recv, Selector),
            Step = fun({K, V}, {Keep, Acc}) ->
                Acc2 = Lambda(V, set_acc_list(ListPath, Acc, [])),
                case get_acc_list(ListPath, Acc2) of
                    [] -> {Keep, Acc2};
                    [Item] -> {[{K, Item} | Keep], Acc2}
                end
            end,
            {Keep, Out} = lists:foldl(Step, {[], Init}, maps:to_list(Recv)),
            set_acc_list(ListPath, Out, Keep);
        false ->
            lists:foldl(Lambda, Init, to_list_for_threading(Recv, Selector))
    end.

-spec get_acc_list([pos_integer()], tuple()) -> term().
get_acc_list([Pos], Acc) -> element(Pos, Acc);
get_acc_list([Pos | Rest], Acc) -> get_acc_list(Rest, element(Pos, Acc)).

-spec set_acc_list([pos_integer()], tuple(), term()) -> tuple().
set_acc_list([Pos], Acc, List) ->
    setelement(Pos, Acc, List);
set_acc_list([Pos | Rest], Acc, List) ->
    setelement(Pos, Acc, set_acc_list(Rest, element(Pos, Acc), List)).

-doc """
Convert any collection to a list by iterating with do:.

Called from compiler-generated Core Erlang for `do:`, `collect:`,
`select:`, `reject:`, and `inject:into:` when the receiver is not
already an Erlang list.

The accumulator is the *caller's mailbox*, not its process dictionary.
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

-doc """
The receiver guard of an inlined list-op fold whose block writes (ADR 0133,
BT-3916): `to_list/1` of the receiver, for the compiled fold to iterate in the
caller's activation, once the receiver is known to answer `Selector` the way
the fold over `do:` does.

That holds only when nothing the stdlib's own method runs through is the
class's own. A receiver whose class resolves `Selector` to a method outside the
stdlib (its own `anySatisfy:`, a package-local trait's `inject:into:`) would
have run that method with a closure that cannot return the write, and folding
over its `do:` instead would answer something else. So does a class that
defines, outside the stdlib, any other collection message (`overrides_collection_selector`
in its class meta, set by codegen from the compiler's one list of them): the
stdlib's `collect:`, `select:`, `count:`, `eachWithIndex:` and `do:separatedBy:`
are `self inject:into:`, and `reject:` is `self select:`, so an override of
those is reached by the pure send and skipped by the fold. Likewise for the
species-dependent messages (`collect:`, `select:`, `reject:`) when `Collection`'s
provision answers them: they end in `self species withAll:`, so a class with
its own `species` or class-side `withAll:` (`overrides_species_construction`) would build a different result than the
fold (BT-3951). All of these raise `stateful_block_dispatch`. A receiver that does not respond to `Selector`
raises `does_not_understand`, as the pure path does. Codegen never emits this
for `do:`, the fold's own source.
""".
-spec to_list_for_threading(term(), atom()) -> list().
to_list_for_threading(Recv, _Selector) when is_list(Recv) ->
    Recv;
to_list_for_threading(#{?BT_CLASS_TAG_KEY := 'Array'}, 'sort:') ->
    %% BT-5090: the in-place fold sorts a plain list, but `Array sort:` answers
    %% an Array, so folding would silently answer the wrong class.
    refuse_writing_fold(
        'Array',
        'sort:',
        [<<" answers an Array from 'sort:', but the in-place fold would answer a List">>]
    );
to_list_for_threading(Recv, Selector) ->
    ok = guard_writing_fold(Recv, Selector),
    to_list(Recv).

-doc """
The checks of `to_list_for_threading/2` without the conversion (BT-5097): `ok`
when the in-place fold may stand in for `Selector` on `Recv`, otherwise the
`stateful_block_dispatch` / `does_not_understand` it raises. `keyed_foldl/5` runs
it once per call for a `Dictionary`, which is folded over its pairs instead of
its `to_list/1`. An inlined writing `to:do:` / `to:by:do:` / `timesRepeat:`
counted loop calls it on a receiver that is not an Integer (BT-5215, BT-5190),
so it raises what the pure send of that selector to the receiver raises.
""".
-spec guard_writing_fold(term(), atom()) -> ok.
guard_writing_fold(Recv, Selector) ->
    {Class, Provider} = selector_provider(Recv, Selector),
    case Provider of
        stdlib ->
            case beamtalk_behaviour_intrinsics:overridesCollectionSelectorFromName(Class) of
                true ->
                    refuse_writing_fold(
                        Class,
                        Selector,
                        [
                            <<
                                " defines a collection message outside the stdlib that the "
                                "stdlib's '"
                            >>,
                            atom_to_binary(Selector, utf8),
                            <<"' may run through; the in-place fold would skip it">>
                        ]
                    );
                false ->
                    case
                        beamtalk_behaviour_intrinsics:speciesConstructionOverriddenFromName(
                            Class, Selector
                        )
                    of
                        false ->
                            guard_species_rebuild(Recv, Class, Selector);
                        true ->
                            refuse_writing_fold(
                                Class,
                                Selector,
                                [
                                    <<
                                        " defines 'species' or a class-side 'withAll:' that the "
                                        "stdlib's '"
                                    >>,
                                    atom_to_binary(Selector, utf8),
                                    <<"' builds its answer with; the in-place fold would skip it">>
                                ]
                            )
                    end
            end;
        none ->
            beamtalk_error:raise(beamtalk_error:new(does_not_understand, Class, Selector));
        dnu ->
            refuse_writing_fold(
                Class,
                Selector,
                [
                    <<" forwards '">>,
                    atom_to_binary(Selector, utf8),
                    <<"' through its own 'doesNotUnderstand:args:', which the in-place fold would skip">>
                ]
            );
        user ->
            refuse_writing_fold(
                Class,
                Selector,
                [
                    <<" answers '">>,
                    atom_to_binary(Selector, utf8),
                    <<"' with a method outside the stdlib, which cannot return that write">>
                ]
            )
    end.

%% BT-5136: a species-built `collect:`/`select:`/`reject:` answers
%% `self species withAll:` on the pure path. `from_list_like/2` rebuilds only
%% Array and Set (String, Binary and Dictionary never reach here); a Bag would
%% answer a raw List instead of a Bag, so refuse. A user `Collection` with the
%% default `species` answers a List on both paths and is allowed.
%% BT-5190: this is an allowlist, not a denylist: a receiver the fold can stand
%% in for is a `Collection` or gets `Selector` from a stdlib trait provision (a
%% fold over `do:`) or the eager `inject:into:` (BT-5263); any other object, a
%% `Stream`, is refused. The check covers map-backed objects only: the clause
%% below passes every other receiver (a List, a tuple) unchecked.
-spec guard_species_rebuild(term(), atom(), atom()) -> ok.
guard_species_rebuild(#{?BT_CLASS_TAG_KEY := Tag}, _Class, _Selector) when
    Tag =:= 'Array'; Tag =:= 'Set'
->
    ok;
guard_species_rebuild(Recv, Class, Selector) when is_map(Recv) ->
    case beamtalk_primitive:is_object(Recv) of
        false ->
            ok;
        true ->
            case beamtalk_behaviour_intrinsics:foldsOverDoFromName(Class, Selector) of
                true -> guard_species_dependent(Class, Selector);
                false -> refuse_not_a_fold_over_do(Class, Selector)
            end
    end;
guard_species_rebuild(_Recv, _Class, _Selector) ->
    ok.

%% BT-5190: the receiver's stdlib `Selector` is its own native method, not a
%% fold over `do:`. An in-place fold would force the receiver (never ending on
%% an infinite `Stream`) and answer a List where the pure path answers a lazy
%% Stream, or short-circuits for `detect:` / `anySatisfy:`.
-spec refuse_not_a_fold_over_do(atom(), atom()) -> no_return().
refuse_not_a_fold_over_do(Class, Selector) ->
    refuse_writing_fold(
        Class,
        Selector,
        [
            <<" answers '">>,
            atom_to_binary(Selector, utf8),
            <<"' with its own method, not a fold over 'do:', which the in-place fold would force and answer as a List">>
        ]
    ).

-spec guard_species_dependent(atom(), atom()) -> ok.
guard_species_dependent(Class, Selector) ->
    case beamtalk_behaviour_intrinsics:speciesDependentThroughCollectionFromName(Class, Selector) of
        true ->
            refuse_writing_fold(
                Class,
                Selector,
                [
                    <<" builds its '">>,
                    atom_to_binary(Selector, utf8),
                    <<"' answer with 'species withAll:', which the in-place fold would answer as a List">>
                ]
            );
        false ->
            ok
    end.

-spec refuse_writing_fold(atom(), atom(), iodata()) -> no_return().
refuse_writing_fold(Class, Selector, Reason) ->
    Error0 = beamtalk_error:new(stateful_block_dispatch, Class, Selector),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"The block assigns a variable of the enclosing method, but ">>,
            atom_to_binary(Class, utf8),
            Reason,
            <<".">>
        ])
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error1,
            <<
                "Compute the value without assigning from the block, or send "
                "'do:' and accumulate there"
            >>
        )
    ).

%% Who answers `Selector` for `Recv`, with the class name to report. A class
%% object answers from its class side: whatever the class defines there is a
%% method the fold would bypass, and the generic `Class` protocol does not
%% define the collection selectors.
-spec selector_provider(term(), atom()) -> {atom(), stdlib | user | dnu | none}.
selector_provider(Recv, Selector) ->
    case beamtalk_class_registry:is_class_object(Recv) of
        true ->
            Name = beamtalk_primitive:class_name_from_tag(element(2, Recv)),
            case beamtalk_primitive:class_responds_to(Name, Selector) of
                true -> {Name, user};
                false -> {Name, class_dnu_provider(Name)}
            end;
        false ->
            Class = beamtalk_primitive:class_of(Recv),
            case beamtalk_behaviour_intrinsics:selectorProviderFromName(Class, Selector) of
                none -> {Class, dnu_provider(Class)};
                Provider -> {Class, Provider}
            end
    end.

%% BT-5236: nothing answers the selector, but a class with its own
%% `doesNotUnderstand:args:` (a forwarding proxy, user or stdlib) would run it on
%% the pure send, which the guard cannot stand in for: report `dnu` so it
%% refuses with `stateful_block_dispatch`.
-spec dnu_provider(atom()) -> dnu | none.
dnu_provider(Class) ->
    case beamtalk_behaviour_intrinsics:overridesDoesNotUnderstandFromName(Class) of
        true -> dnu;
        false -> none
    end.

%% BT-5270: the class-object counterpart of `dnu_provider/1`. A class-side
%% `doesNotUnderstand:args:` (own or a superclass's) receives the unknown message
%% on the pure send. The class-side tables exclude the `ProtoObject` default.
-spec class_dnu_provider(atom()) -> dnu | none.
class_dnu_provider(Name) ->
    case
        beamtalk_class_dispatch:class_understands_class_selector(Name, 'doesNotUnderstand:args:')
    of
        true -> dnu;
        false -> none
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
