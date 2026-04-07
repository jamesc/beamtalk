%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ets).

%%% **DDD Context:** Object System Context

-moduledoc """
Ets class implementation — shared in-memory tables via OTP ets.

Ets is an Erlang-backed class wrapping OTP ets tables. Tables are
named and public by default, enabling cross-actor reads and writes
without message-passing overhead. The owning process holds the table;
when the owner terminates, the table is deleted automatically.
Only the owner process may call `deleteTable/1`.

Ets objects are represented as tagged maps:
```
#{
  '$beamtalk_class' => 'Ets',
  table => TableName :: atom()
}
```

## Class Methods

| Selector      | Description                                       |
|---------------|---------------------------------------------------|
| `new:type:`   | Create a named public ETS table                   |
| `named:`      | Look up an existing named table by atom name      |

## Instance Methods

| Selector           | Description                                  |
|--------------------|----------------------------------------------|
| `lookup:key:`      | Look up a key; nil if absent                 |
| `insert:key:value:`| Insert or update a key-value pair            |
| `lookupIfAbsent:key:block:` | Lookup with default block           |
| `includesKey:key:` | Test key membership                          |
| `removeKey:key:`   | Delete an entry; returns nil                 |
| `keys:`            | Return all keys as a List                    |
| `tableSize:`       | Number of entries                            |
| `deleteTable:`     | Destroy the table                            |

## References

- BT-1189: ETS — shared in-memory table class for actor state sharing
- ADR 0042: Actor-Only Mutable State
- ADR 0055: Erlang-Backed Class Protocol
""".

%% Class methods — canonical colon forms (for EUnit tests and Beamtalk dispatch)
-export(['new:type:'/2, 'named:'/1, 'exists:'/1, 'newOrExisting:type:'/2]).

%% Instance methods — no-colon forms used by the FFI proxy
%% The (Erlang beamtalk_ets) dispatch strips the first keyword from the selector
%% e.g. `lookup: self key: key` → selector `lookup:key:` → function `lookup/2`
-export([lookup/2, insert/3, lookupIfAbsent/3, includesKey/2, removeKey/2]).
-export([keys/1, tableSize/1, deleteTable/1]).

%% FFI shims for class methods: `(Erlang beamtalk_ets) new: name type: t` → `new/2`
-export([new/2, named/1, exists/1, newOrExisting/2]).

-type t() :: #{'$beamtalk_class' := 'Ets', atom() => term()}.
-export_type([t/0]).

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

-doc """
Create a new named public ETS table.

`TableType` must be one of: `set`, `orderedSet`, `bag`, `duplicateBag`.
The table is created with `named_table` and `public` access so it can be
read and written by any process.

Raises `already_exists` if a table with the same name already exists.
Raises `type_error` if arguments are not atoms.
""".
-spec 'new:type:'(atom(), atom()) -> t().
'new:type:'(Name, TableType) when is_atom(Name), is_atom(TableType) ->
    EtsType = map_table_type(TableType),
    try
        ets:new(Name, [EtsType, named_table, public, {keypos, 1}]),
        make_ets(Name)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(already_exists, 'Ets'),
            Error1 = beamtalk_error:with_selector(Error0, 'new:type:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"A table with this name already exists">>
            ),
            beamtalk_error:raise(Error2)
    end;
'new:type:'(Name, _TableType) when not is_atom(Name) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'new:type:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Table name must be a Symbol">>),
    beamtalk_error:raise(Error2);
'new:type:'(_Name, _TableType) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'new:type:'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<"Table type must be a Symbol (#set, #orderedSet, #bag, #duplicateBag)">>
    ),
    beamtalk_error:raise(Error2).

-doc """
Look up an existing named ETS table.

Returns an Ets instance if a table with the given name exists.
Raises `not_found` if no table with that name has been created.
Raises `type_error` if the argument is not an atom.
""".
-spec 'named:'(atom()) -> t().
'named:'(Name) when is_atom(Name) ->
    case ets:whereis(Name) of
        undefined ->
            Error0 = beamtalk_error:new(not_found, 'Ets'),
            Error1 = beamtalk_error:with_selector(Error0, 'named:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"No ETS table with this name exists">>
            ),
            beamtalk_error:raise(Error2);
        _Tid ->
            make_ets(Name)
    end;
'named:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'named:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Table name must be a Symbol">>),
    beamtalk_error:raise(Error2).

-doc """
Check whether a named ETS table exists.

Returns `true` if a table with the given name exists, `false` otherwise.
Raises `type_error` if the argument is not an atom.
""".
-spec 'exists:'(atom()) -> boolean().
'exists:'(Name) when is_atom(Name) ->
    ets:whereis(Name) =/= undefined;
'exists:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'exists:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Table name must be a Symbol">>),
    beamtalk_error:raise(Error2).

-doc """
Create a named table or return the existing one.

If a table with the given name already exists, returns an Ets instance
wrapping it. Otherwise creates a new table with the specified type.
Raises `type_error` if arguments are not atoms.
""".
-spec 'newOrExisting:type:'(atom(), atom()) -> t().
'newOrExisting:type:'(Name, TableType) when is_atom(Name), is_atom(TableType) ->
    EtsType = map_table_type_for('newOrExisting:type:', TableType),
    try
        ets:new(Name, [EtsType, named_table, public, {keypos, 1}]),
        make_ets(Name)
    catch
        error:badarg ->
            %% Table was created by another process concurrently — adopt it.
            case ets:whereis(Name) of
                undefined ->
                    %% Table vanished between ets:new and ets:whereis — re-raise.
                    Error0 = beamtalk_error:new(already_exists, 'Ets'),
                    Error1 = beamtalk_error:with_selector(Error0, 'newOrExisting:type:'),
                    Error2 = beamtalk_error:with_hint(
                        Error1,
                        <<"Failed to create or find table">>
                    ),
                    beamtalk_error:raise(Error2);
                _Tid ->
                    make_ets(Name)
            end
    end;
'newOrExisting:type:'(Name, _TableType) when not is_atom(Name) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'newOrExisting:type:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Table name must be a Symbol">>),
    beamtalk_error:raise(Error2);
'newOrExisting:type:'(_Name, _TableType) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'newOrExisting:type:'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<"Table type must be a Symbol (#set, #orderedSet, #bag, #duplicateBag)">>
    ),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Instance Methods
%%%
%%% All instance methods receive the Ets tagged map as the first argument.
%%% The FFI proxy calls these by stripping the first keyword from the selector:
%%%   `lookup: self key: key` → selector `lookup:key:` → `lookup(Self, Key)`
%%% ============================================================================

-doc """
Look up a key. Returns the value, or nil if the key is absent.

For bag/duplicate_bag tables with multiple values for a key, returns
the first stored value. The wrapper exposes only one value per key.
""".
-spec lookup(t(), term()) -> term().
lookup(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    try
        case ets:lookup(TableName, Key) of
            [{_K, Value} | _] -> Value;
            [] -> nil
        end
    catch
        error:badarg -> stale_table_error('lookup:key:', TableName)
    end;
lookup(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc """
Insert or update a key-value pair. Returns nil.

For bag/duplicate_bag tables, deletes all existing entries for Key before
inserting, giving best-effort upsert semantics. Note: the delete and insert
are two separate ETS operations. Concurrent writes from multiple actors to
the same key on a bag table are not serialized — interleaved calls may
result in multiple entries for the same key.
""".
-spec insert(t(), term(), term()) -> nil.
insert(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Value) ->
    try
        case ets:info(TableName, type) of
            T when T =:= bag; T =:= duplicate_bag ->
                ets:delete(TableName, Key);
            _ ->
                ok
        end,
        ets:insert(TableName, {Key, Value}),
        nil
    catch
        error:badarg -> stale_table_error('insert:key:value:', TableName)
    end;
insert(_Self, _Key, _Value) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc "Look up a key; if absent, evaluate the block and return its result.".
-spec lookupIfAbsent(t(), term(), function()) -> term().
lookupIfAbsent(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Block) when
    is_function(Block, 0)
->
    try
        case ets:lookup(TableName, Key) of
            [{_K, Value} | _] -> Value;
            [] -> Block()
        end
    catch
        error:badarg -> stale_table_error('lookupIfAbsent:key:block:', TableName)
    end;
lookupIfAbsent(#{'$beamtalk_class' := 'Ets'}, _Key, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:ifAbsent:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Third argument must be a Block">>),
    beamtalk_error:raise(Error2);
lookupIfAbsent(_Self, _Key, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:ifAbsent:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc "Test whether a key exists in the table.".
-spec includesKey(t(), term()) -> boolean().
includesKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    try
        ets:member(TableName, Key)
    catch
        error:badarg -> stale_table_error('includesKey:key:', TableName)
    end;
includesKey(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'includesKey:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc "Remove the entry for key. Returns nil.".
-spec removeKey(t(), term()) -> nil.
removeKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    try
        ets:delete(TableName, Key),
        nil
    catch
        error:badarg -> stale_table_error('removeKey:key:', TableName)
    end;
removeKey(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'removeKey:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc """
Return all unique keys in the table as a List.

For bag/duplicate_bag tables, duplicate keys are collapsed so each key
appears only once. Order is unspecified for set/bag tables; sorted for orderedSet.
""".
-spec keys(t()) -> list().
keys(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    try
        lists:usort(ets:select(TableName, [{{'$1', '_'}, [], ['$1']}]))
    catch
        error:badarg -> stale_table_error('keys:', TableName)
    end;
keys(_Self) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'keys'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc """
Return the number of entries in the table.

Named `tableSize` to avoid shadowing the deprecated `erlang:size/1` BIF.
""".
-spec tableSize(t()) -> non_neg_integer().
tableSize(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    case ets:info(TableName, size) of
        undefined -> stale_table_error('tableSize:', TableName);
        Size -> Size
    end;
tableSize(_Self) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'size'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

-doc """
Destroy the ETS table. Returns nil.

Named `deleteTable` to avoid shadowing `ets:delete/1` in call sites.
""".
-spec deleteTable(t()) -> nil.
deleteTable(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    try
        ets:delete(TableName),
        nil
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(permission_error, 'Ets'),
            Error1 = beamtalk_error:with_selector(Error0, 'delete'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Caller is not the owner of the ETS table, or the table has already been deleted">>
            ),
            beamtalk_error:raise(Error2)
    end;
deleteTable(_Self) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'delete'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_ets) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector. These shims provide the colon-free entry points:
%%%   `(Erlang beamtalk_ets) new: name type: t` → `new(name, t)` → `'new:type:'`
%%%   `(Erlang beamtalk_ets) named: name`       → `named(name)` → `'named:'`
%%% ============================================================================

-doc "FFI shim for new:type: — called via (Erlang beamtalk_ets) new: name type: type.".
-spec new(atom(), atom()) -> t().
new(Name, TableType) -> 'new:type:'(Name, TableType).

-doc "FFI shim for named: — called via (Erlang beamtalk_ets) named: name.".
-spec named(atom()) -> t().
named(Name) -> 'named:'(Name).

-doc "FFI shim for exists: — called via (Erlang beamtalk_ets) exists: name.".
-spec exists(atom()) -> boolean().
exists(Name) -> 'exists:'(Name).

-doc """
FFI shim for newOrExisting:type: — called via (Erlang beamtalk_ets) newOrExisting: name type: t.
""".
-spec newOrExisting(atom(), atom()) -> t().
newOrExisting(Name, TableType) -> 'newOrExisting:type:'(Name, TableType).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc """
Build a structured error for a stale or deleted ETS table.

Used when an ETS operation raises `error:badarg` because the table no
longer exists (owner terminated or `deleteTable/1` was called).
""".
-spec stale_table_error(atom(), atom()) -> no_return().
stale_table_error(Selector, TableName) ->
    Error0 = beamtalk_error:new(stale_table, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"ETS table is stale or has been deleted: ">>,
            atom_to_binary(TableName, utf8)
        ])
    ),
    beamtalk_error:raise(Error2).

-doc "Build an Ets tagged map for the given table name.".
-spec make_ets(atom()) -> t().
make_ets(Name) ->
    #{'$beamtalk_class' => 'Ets', table => Name}.

-doc """
Map Beamtalk table type symbols to OTP ets type atoms.

Beamtalk uses camelCase symbols matching the Smalltalk convention.
OTP uses snake_case atoms.
""".
-spec map_table_type(atom()) -> set | ordered_set | bag | duplicate_bag.
map_table_type(TableType) ->
    map_table_type_for('new:type:', TableType).

-doc """
Map Beamtalk table type symbols to OTP ets type atoms, with caller selector for errors.
""".
-spec map_table_type_for(atom(), atom()) -> set | ordered_set | bag | duplicate_bag.
map_table_type_for(_Selector, set) ->
    set;
map_table_type_for(_Selector, orderedSet) ->
    ordered_set;
map_table_type_for(_Selector, bag) ->
    bag;
map_table_type_for(_Selector, duplicateBag) ->
    duplicate_bag;
map_table_type_for(Selector, Other) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{type => Other}),
    Error3 = beamtalk_error:with_hint(
        Error2,
        <<"Table type must be one of: #set, #orderedSet, #bag, #duplicateBag">>
    ),
    beamtalk_error:raise(Error3).
