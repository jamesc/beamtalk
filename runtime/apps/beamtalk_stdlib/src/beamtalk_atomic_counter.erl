%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc AtomicCounter class implementation — named atomic integer counter via ETS.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% AtomicCounter provides a shared integer counter that can be safely incremented
%%% and decremented from multiple actors without message-passing overhead.
%%% Backed by `ets:update_counter`, which is an atomic ETS operation.
%%%
%%% Each counter owns its own named ETS table. The table stores a single entry
%%% `{value, N :: integer()}`. Atomic operations use `ets:update_counter/3` which
%%% is serialized by the ETS implementation and safe for concurrent access.
%%%
%%% AtomicCounter objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'AtomicCounter',
%%%   table => TableName :: atom()
%%% }
%%% ```
%%%
%%% ## Class Methods
%%%
%%% | Selector   | Description                                         |
%%% |------------|-----------------------------------------------------|
%%% | `new:`     | Create a named counter starting at 0               |
%%% | `named:`   | Look up an existing counter by name                |
%%%
%%% ## Instance Methods
%%%
%%% | Selector        | Description                                   |
%%% |-----------------|-----------------------------------------------|
%%% | `increment`     | Atomically add 1; returns new value           |
%%% | `incrementBy:`  | Atomically add N; returns new value           |
%%% | `decrement`     | Atomically subtract 1; returns new value      |
%%% | `decrementBy:`  | Atomically subtract N; returns new value      |
%%% | `value`         | Read current value                            |
%%% | `reset`         | Set to 0; returns nil                         |
%%% | `delete`        | Destroy the backing ETS table; returns nil    |
%%%
%%% ## References
%%%
%%% - BT-1250: stdlib: mutable collections — Queue and AtomicCounter
%%% - `ets:update_counter/3` for atomic increment
%%% - Existing FFI pattern: `beamtalk_ets.erl`

-module(beamtalk_atomic_counter).

%% Class methods — canonical colon forms (for EUnit tests and Beamtalk dispatch)
-export(['new:'/1, 'named:'/1]).

%% Instance methods — no-colon forms used by the FFI proxy
-export([increment/1, incrementBy/2, decrement/1, decrementBy/2, value/1, reset/1, delete/1]).
%% readValue/1: FFI shim for value/1 — avoids the `value:` block-eval codegen intercept.
%% The Beamtalk method `value` dispatches via `(Erlang beamtalk_atomic_counter) readValue: self`
%% to work around BT-1251 (codegen intercepts unary `value:` as block evaluation).
-export([readValue/1]).

%% FFI shims for class methods
-export([new/1, named/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

%% @doc Create a new named counter starting at 0.
%%
%% Creates a named ETS table with a single entry `{value, 0}`.
%% Raises `already_exists` if a counter with this name already exists.
%% Raises `type_error` if the argument is not an atom.
-spec 'new:'(atom()) -> map().
'new:'(Name) when is_atom(Name) ->
    try
        ets:new(Name, [set, named_table, public, {keypos, 1}]),
        ets:insert(Name, {value, 0}),
        make_counter(Name)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(already_exists, 'AtomicCounter'),
            Error1 = beamtalk_error:with_selector(Error0, 'new:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"A counter with this name already exists">>
            ),
            beamtalk_error:raise(Error2)
    end;
'new:'(_Name) ->
    Error0 = beamtalk_error:new(type_error, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, 'new:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Counter name must be a Symbol">>),
    beamtalk_error:raise(Error2).

%% @doc Look up an existing named counter.
%%
%% Returns an AtomicCounter instance if a counter with the given name exists.
%% Raises `not_found` if no counter with that name has been created.
%% Raises `type_error` if the argument is not an atom.
-spec 'named:'(atom()) -> map().
'named:'(Name) when is_atom(Name) ->
    case ets:whereis(Name) of
        undefined ->
            Error0 = beamtalk_error:new(not_found, 'AtomicCounter'),
            Error1 = beamtalk_error:with_selector(Error0, 'named:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"No counter with this name exists">>
            ),
            beamtalk_error:raise(Error2);
        _Tid ->
            make_counter(Name)
    end;
'named:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, 'named:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Counter name must be a Symbol">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Instance Methods
%%%
%%% All instance methods receive the AtomicCounter tagged map as the first
%%% argument. The FFI proxy calls these by stripping the first keyword:
%%%   `increment: self`         → selector `increment:`  → `increment(Self)`
%%%   `incrementBy: self by: n` → selector `incrementBy:by:` → `incrementBy(Self, N)`
%%% ============================================================================

%% @doc Atomically add 1. Returns the new value.
-spec increment(map()) -> integer().
increment(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}) ->
    try
        ets:update_counter(TableName, value, 1)
    catch
        error:badarg -> stale_counter_error('increment', TableName)
    end;
increment(_Self) ->
    receiver_type_error('increment').

%% @doc Atomically add N. Returns the new value.
-spec incrementBy(map(), integer()) -> integer().
incrementBy(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}, N) when is_integer(N) ->
    try
        ets:update_counter(TableName, value, N)
    catch
        error:badarg -> stale_counter_error('incrementBy:', TableName)
    end;
incrementBy(#{'$beamtalk_class' := 'AtomicCounter'}, _N) ->
    Error0 = beamtalk_error:new(type_error, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, 'incrementBy:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be an Integer">>),
    beamtalk_error:raise(Error2);
incrementBy(_Self, _N) ->
    receiver_type_error('incrementBy:').

%% @doc Atomically subtract 1. Returns the new value.
-spec decrement(map()) -> integer().
decrement(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}) ->
    try
        ets:update_counter(TableName, value, -1)
    catch
        error:badarg -> stale_counter_error('decrement', TableName)
    end;
decrement(_Self) ->
    receiver_type_error('decrement').

%% @doc Atomically subtract N. Returns the new value.
-spec decrementBy(map(), integer()) -> integer().
decrementBy(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}, N) when is_integer(N) ->
    try
        ets:update_counter(TableName, value, -N)
    catch
        error:badarg -> stale_counter_error('decrementBy:', TableName)
    end;
decrementBy(#{'$beamtalk_class' := 'AtomicCounter'}, _N) ->
    Error0 = beamtalk_error:new(type_error, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, 'decrementBy:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be an Integer">>),
    beamtalk_error:raise(Error2);
decrementBy(_Self, _N) ->
    receiver_type_error('decrementBy:').

%% @doc Read the current value.
-spec value(map()) -> integer().
value(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}) ->
    try
        case ets:lookup(TableName, value) of
            [{value, N}] -> N;
            [] -> stale_counter_error('value', TableName)
        end
    catch
        error:badarg -> stale_counter_error('value', TableName)
    end;
value(_Self) ->
    receiver_type_error('value').

%% @doc FFI shim for value/1 — called via (Erlang beamtalk_atomic_counter) readValue: self.
%%
%% Exists to work around BT-1251: the Beamtalk codegen intercepts the keyword
%% selector `value:` as a block evaluation before the Erlang FFI path is reached.
%% Using `readValue:` bypasses the intercept and calls this shim, which delegates
%% to `value/1`.
-spec readValue(map()) -> integer().
readValue(Self) -> value(Self).

%% @doc Reset the counter to 0. Returns nil.
-spec reset(map()) -> nil.
reset(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}) ->
    try
        ets:insert(TableName, {value, 0}),
        nil
    catch
        error:badarg -> stale_counter_error('reset', TableName)
    end;
reset(_Self) ->
    receiver_type_error('reset').

%% @doc Destroy the backing ETS table. Returns nil.
-spec delete(map()) -> nil.
delete(#{'$beamtalk_class' := 'AtomicCounter', table := TableName}) ->
    try
        ets:delete(TableName),
        nil
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(permission_error, 'AtomicCounter'),
            Error1 = beamtalk_error:with_selector(Error0, 'delete'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Caller is not the owner of the counter table, or it has already been deleted">>
            ),
            beamtalk_error:raise(Error2)
    end;
delete(_Self) ->
    receiver_type_error('delete').

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_atomic_counter) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the selector.
%%%   `(Erlang beamtalk_atomic_counter) new: name` → `new(name)` → `'new:'`
%%%   `(Erlang beamtalk_atomic_counter) named: name` → `named(name)` → `'named:'`
%%% ============================================================================

%% @doc FFI shim for new: — called via (Erlang beamtalk_atomic_counter) new: name.
-spec new(atom()) -> map().
new(Name) -> 'new:'(Name).

%% @doc FFI shim for named: — called via (Erlang beamtalk_atomic_counter) named: name.
-spec named(atom()) -> map().
named(Name) -> 'named:'(Name).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private
%% @doc Build an AtomicCounter tagged map for the given table name.
-spec make_counter(atom()) -> map().
make_counter(Name) ->
    #{'$beamtalk_class' => 'AtomicCounter', table => Name}.

%% @private
%% @doc Build a structured error for a stale or deleted counter table.
-spec stale_counter_error(atom(), atom()) -> no_return().
stale_counter_error(Selector, TableName) ->
    Error0 = beamtalk_error:new(stale_counter, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"AtomicCounter is stale or has been deleted: ">>,
            atom_to_binary(TableName, utf8)
        ])
    ),
    beamtalk_error:raise(Error2).

%% @private
%% @doc Raise a type_error for a non-AtomicCounter receiver.
-spec receiver_type_error(atom()) -> no_return().
receiver_type_error(Selector) ->
    Error0 = beamtalk_error:new(type_error, 'AtomicCounter'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an AtomicCounter instance">>),
    beamtalk_error:raise(Error2).
