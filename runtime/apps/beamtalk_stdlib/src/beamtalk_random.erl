%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_random).

%%% **DDD Context:** Object System Context

-moduledoc """
Random class implementation — random number generation via Erlang `rand`.

Random provides both class-side convenience methods (using the process
dictionary seed) and instance-side methods (using explicit state for
reproducibility and isolation).

Wraps Erlang's `rand` module. Default algorithm is `exsss` (Xorshift116**).

**NOT cryptographically secure.**

## Class Methods

| Selector        | Description                              |
|-----------------|------------------------------------------|
| `next`          | Random float 0.0..1.0 (process dict)     |
| `nextInteger:`  | Random integer 1..N (process dict)       |
| `new`           | New auto-seeded instance                 |
| `seed:`         | New instance with deterministic seed     |

## Instance Methods

| Selector             | Description                         |
|----------------------|-------------------------------------|
| `next`               | Random float, pure on instance state|
| `nextInteger:`        | Random integer, pure on instance state|

## FFI Shims

The FFI / `native:` dispatch derives the Erlang function name from the first
keyword of the Beamtalk selector (stripping the trailing colon). The class-side
shims (`nextInteger/1`, `seed/1`) bridge the gap so that
`Random nextInteger: max` dispatches to the canonical `'nextInteger:'/1`
implementation. The instance-side `next`/`nextInteger:` selectors delegate
directly to `next/1` / `nextInteger/2` (ADR 0101 / BT-2731).
""".

-export(['next'/0, 'nextInteger:'/1, 'new'/0, 'seed:'/1]).
%% Instance-side `native:` delegation targets (ADR 0101 / BT-2731): the first
%% keyword + self-threading rule maps `rng next` → `next/1` and
%% `rng nextInteger: n` → `nextInteger/2`. They coexist with the arity-0/1
%% class-side `next/0` / `nextInteger/1` (arity disambiguates).
-export([next/1, nextInteger/2]).
-export(['atRandom'/1]).

%% FFI shims for (Erlang beamtalk_random) dispatch
-export([nextInteger/1, seed/1]).

-type t() :: #{'$beamtalk_class' := 'Random', atom() => term()}.
-export_type([t/0]).

%%% ============================================================================
%%% Class-side Methods (process dictionary seed)
%%% ============================================================================

-doc "Return a random float between 0.0 (inclusive) and 1.0 (exclusive).".
-spec 'next'() -> float().
'next'() ->
    rand:uniform().

-doc "Return a random integer between 1 and Max (inclusive).".
-spec 'nextInteger:'(integer()) -> integer().
'nextInteger:'(Max) when is_integer(Max), Max > 0 ->
    rand:uniform(Max);
'nextInteger:'(Max) when is_integer(Max) ->
    beamtalk_error:raise_type_error(
        'Random', 'nextInteger:', <<"Argument must be a positive Integer">>
    );
'nextInteger:'(_) ->
    beamtalk_error:raise_type_error('Random', 'nextInteger:', <<"Argument must be an Integer">>).

%%% ============================================================================
%%% Class-side Constructors
%%% ============================================================================

-doc "Create a new Random instance with an auto-generated seed.".
-spec 'new'() -> t().
'new'() ->
    State = rand:seed_s(exsss),
    #{
        '$beamtalk_class' => 'Random',
        state => State
    }.

-doc "Create a new Random instance with a specific seed.".
-spec 'seed:'(integer()) -> t().
'seed:'(Seed) when is_integer(Seed) ->
    State = rand:seed_s(exsss, {Seed, Seed, Seed}),
    #{
        '$beamtalk_class' => 'Random',
        state => State
    };
'seed:'(_) ->
    beamtalk_error:raise_type_error('Random', 'seed:', <<"Seed must be an Integer">>).

%%% ============================================================================
%%% Instance Methods (explicit state)
%%% ============================================================================

-doc """
Return a random float from instance state.
Random instances are immutable value types — calling next on the same
instance always returns the same value.
""".
-spec next(t()) -> float().
next(#{state := State0}) ->
    {Value, _State1} = rand:uniform_s(State0),
    Value;
next(_) ->
    beamtalk_error:raise_type_error('Random', 'next', <<"Receiver must be a Random instance">>).

-doc """
Return a random integer between 1 and Max from instance state.
Random instances are immutable value types — calling nextInteger: on
the same instance always returns the same value.
""".
-spec nextInteger(t(), integer()) -> integer().
nextInteger(#{state := State0}, Max) when is_integer(Max), Max > 0 ->
    {Value, _State1} = rand:uniform_s(Max, State0),
    Value;
nextInteger(#{state := _}, Max) when is_integer(Max) ->
    beamtalk_error:raise_type_error(
        'Random', 'nextInteger:', <<"Argument must be a positive Integer">>
    );
nextInteger(#{state := _}, _) ->
    beamtalk_error:raise_type_error('Random', 'nextInteger:', <<"Argument must be an Integer">>);
nextInteger(_, _) ->
    beamtalk_error:raise_type_error(
        'Random', 'nextInteger:', <<"Receiver must be a Random instance">>
    ).

%%% ============================================================================
%%% Collection Integration
%%% ============================================================================

-doc """
Return a random element from a list or tuple.
Used by List atRandom and Tuple atRandom.
""".
-spec 'atRandom'(list() | tuple()) -> term().
'atRandom'([]) ->
    beamtalk_error:raise_type_error(
        'List', 'atRandom', <<"Cannot select random element from empty collection">>
    );
'atRandom'(List) when is_list(List) ->
    Len = length(List),
    Index = rand:uniform(Len),
    lists:nth(Index, List);
'atRandom'(Tuple) when is_tuple(Tuple), tuple_size(Tuple) > 0 ->
    Index = rand:uniform(tuple_size(Tuple)),
    element(Index, Tuple);
'atRandom'(Tuple) when is_tuple(Tuple) ->
    beamtalk_error:raise_type_error(
        'Tuple', 'atRandom', <<"Cannot select random element from empty collection">>
    ).

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_random) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector (stripping the trailing colon). These shims provide
%%% the colon-free entry points that the proxy calls:
%%%
%%%   (Erlang beamtalk_random) nextInteger: max  → nextInteger/1
%%%   (Erlang beamtalk_random) seed: seed        → seed/1
%%%
%%% The class-side `next`/`new` and the instance-side `next`/`nextInteger:`
%%% delegations need no shim: their first keyword already names an exported
%%% function (`next/0`, `new/0`, `next/1`, `nextInteger/2` — see the `native:`
%%% `self delegate` methods on the Random class, ADR 0101 / BT-2731).
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_random) nextInteger: max`".
-spec nextInteger(integer()) -> integer().
nextInteger(Max) -> 'nextInteger:'(Max).

-doc "FFI shim: `(Erlang beamtalk_random) seed: seed`".
-spec seed(integer()) -> t().
seed(Seed) -> 'seed:'(Seed).
