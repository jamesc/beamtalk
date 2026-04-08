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

The `beamtalk_erlang_proxy:direct_call/3` dispatch derives the Erlang function
name from the first keyword of the Beamtalk selector (stripping the trailing
colon). The shims (`nextInteger/1`, `seed/1`, `instanceNextInteger/2`) bridge
the gap so that `(Erlang beamtalk_random) nextInteger: max` dispatches
correctly to the canonical `'nextInteger:'/1` implementation.
""".

-export(['next'/0, 'nextInteger:'/1, 'new'/0, 'seed:'/1]).
-export(['instanceNext'/1, 'instanceNextInteger:'/2]).
-export(['atRandom'/1]).

%% FFI shims for (Erlang beamtalk_random) dispatch
-export([nextInteger/1, seed/1, instanceNextInteger/2]).

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
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'nextInteger:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a positive Integer">>),
    beamtalk_error:raise(Error2);
'nextInteger:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'nextInteger:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be an Integer">>),
    beamtalk_error:raise(Error2).

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
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'seed:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Seed must be an Integer">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Instance Methods (explicit state)
%%% ============================================================================

-doc """
Return a random float from instance state.
Random instances are immutable value types — calling next on the same
instance always returns the same value.
""".
-spec 'instanceNext'(t()) -> float().
'instanceNext'(#{state := State0}) ->
    {Value, _State1} = rand:uniform_s(State0),
    Value;
'instanceNext'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'next'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a Random instance">>),
    beamtalk_error:raise(Error2).

-doc """
Return a random integer between 1 and Max from instance state.
Random instances are immutable value types — calling nextInteger: on
the same instance always returns the same value.
""".
-spec 'instanceNextInteger:'(t(), integer()) -> integer().
'instanceNextInteger:'(#{state := State0}, Max) when is_integer(Max), Max > 0 ->
    {Value, _State1} = rand:uniform_s(Max, State0),
    Value;
'instanceNextInteger:'(#{state := _}, Max) when is_integer(Max) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'nextInteger:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a positive Integer">>),
    beamtalk_error:raise(Error2);
'instanceNextInteger:'(#{state := _}, _) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'nextInteger:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be an Integer">>),
    beamtalk_error:raise(Error2);
'instanceNextInteger:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'nextInteger:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a Random instance">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Collection Integration
%%% ============================================================================

-doc """
Return a random element from a list or tuple.
Used by List atRandom and Tuple atRandom.
""".
-spec 'atRandom'(list() | tuple()) -> term().
'atRandom'([]) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'atRandom'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"Cannot select random element from empty collection">>
    ),
    beamtalk_error:raise(Error2);
'atRandom'(List) when is_list(List) ->
    Len = length(List),
    Index = rand:uniform(Len),
    lists:nth(Index, List);
'atRandom'(Tuple) when is_tuple(Tuple), tuple_size(Tuple) > 0 ->
    Index = rand:uniform(tuple_size(Tuple)),
    element(Index, Tuple);
'atRandom'(Tuple) when is_tuple(Tuple) ->
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'atRandom'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"Cannot select random element from empty collection">>
    ),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_random) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector (stripping the trailing colon). These shims provide
%%% the colon-free entry points that the proxy calls:
%%%
%%%   (Erlang beamtalk_random) next              → next/0  (direct)
%%%   (Erlang beamtalk_random) nextInteger: max  → nextInteger/1
%%%   (Erlang beamtalk_random) new               → new/0   (direct)
%%%   (Erlang beamtalk_random) seed: seed        → seed/1
%%%   (Erlang beamtalk_random) instanceNext: self            → instanceNext/1 (direct)
%%%   (Erlang beamtalk_random) instanceNextInteger: self with: max → instanceNextInteger/2
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_random) nextInteger: max`".
-spec nextInteger(integer()) -> integer().
nextInteger(Max) -> 'nextInteger:'(Max).

-doc "FFI shim: `(Erlang beamtalk_random) seed: seed`".
-spec seed(integer()) -> t().
seed(Seed) -> 'seed:'(Seed).

-doc "FFI shim: `(Erlang beamtalk_random) instanceNextInteger: self with: max`".
-spec instanceNextInteger(t(), integer()) -> integer().
instanceNextInteger(Self, Max) -> 'instanceNextInteger:'(Self, Max).
