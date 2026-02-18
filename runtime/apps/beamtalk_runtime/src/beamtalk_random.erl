%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Random class implementation — random number generation via Erlang `rand`.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Random provides both class-side convenience methods (using the process
%%% dictionary seed) and instance-side methods (using explicit state for
%%% reproducibility and isolation).
%%%
%%% Wraps Erlang's `rand` module. Default algorithm is `exsss` (Xorshift116**).
%%%
%%% **NOT cryptographically secure.**
%%%
%%% ## Class Methods
%%%
%%% | Selector        | Description                              |
%%% |-----------------|------------------------------------------|
%%% | `next`          | Random float 0.0..1.0 (process dict)     |
%%% | `nextInteger:`  | Random integer 1..N (process dict)       |
%%% | `new`           | New auto-seeded instance                 |
%%% | `seed:`         | New instance with deterministic seed     |
%%%
%%% ## Instance Methods
%%%
%%% | Selector             | Description                         |
%%% |----------------------|-------------------------------------|
%%% | `next`               | Random float, pure on instance state|
%%% | `nextInteger:`        | Random integer, pure on instance state|
%%% | `printString`        | String representation               |

-module(beamtalk_random).

-export(['next'/0, 'nextInteger:'/1, 'new'/0, 'seed:'/1]).
-export(['instanceNext'/1, 'instanceNextInteger:'/2, 'printString'/1]).
-export(['atRandom'/1]).
-export([has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Class-side Methods (process dictionary seed)
%%% ============================================================================

%% @doc Return a random float between 0.0 (inclusive) and 1.0 (exclusive).
-spec 'next'() -> float().
'next'() ->
    rand:uniform().

%% @doc Return a random integer between 1 and Max (inclusive).
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

%% @doc Create a new Random instance with an auto-generated seed.
-spec 'new'() -> map().
'new'() ->
    State = rand:seed_s(exsss),
    #{
        '$beamtalk_class' => 'Random',
        state => State
    }.

%% @doc Create a new Random instance with a specific seed.
-spec 'seed:'(integer()) -> map().
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

%% @doc Return a random float from instance state.
%% Random instances are immutable value types — calling next on the same
%% instance always returns the same value.
-spec 'instanceNext'(map()) -> float().
'instanceNext'(#{state := State0}) ->
    {Value, _State1} = rand:uniform_s(State0),
    Value;
'instanceNext'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Random'),
    Error1 = beamtalk_error:with_selector(Error0, 'next'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a Random instance">>),
    beamtalk_error:raise(Error2).

%% @doc Return a random integer between 1 and Max from instance state.
%% Random instances are immutable value types — calling nextInteger: on
%% the same instance always returns the same value.
-spec 'instanceNextInteger:'(map(), integer()) -> integer().
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

%% @doc Return a developer-readable string representation.
-spec 'printString'(map()) -> binary().
'printString'(#{'$beamtalk_class' := 'Random'}) ->
    <<"a Random">>.

%%% ============================================================================
%%% Collection Integration
%%% ============================================================================

%% @doc Return a random element from a list or tuple.
%% Used by List atRandom and Tuple atRandom.
-spec 'atRandom'(list() | tuple()) -> term().
'atRandom'([]) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'atRandom'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Cannot select random element from empty collection">>),
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
    Error2 = beamtalk_error:with_hint(Error1, <<"Cannot select random element from empty collection">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Method Dispatch
%%% ============================================================================

%% @doc Check if Random responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('next') -> true;
has_method('nextInteger:') -> true;
has_method('new') -> true;
has_method('seed:') -> true;
has_method('instanceNext') -> true;
has_method('instanceNextInteger:') -> true;
has_method('printString') -> true;
has_method(_) -> false.
