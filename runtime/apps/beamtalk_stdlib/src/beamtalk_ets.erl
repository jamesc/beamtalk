%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Ets class implementation — shared in-memory tables via OTP ets.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Ets provides a value-type wrapper around OTP ets tables. Tables are
%%% named and public by default, enabling cross-actor reads and writes
%%% without message-passing overhead. The owning process holds the table;
%%% when the owner terminates, the table is deleted automatically.
%%%
%%% Ets objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Ets',
%%%   table => TableName :: atom()
%%% }
%%% ```
%%%
%%% ## Class Methods
%%%
%%% | Selector      | Description                                       |
%%% |---------------|---------------------------------------------------|
%%% | `new:type:`   | Create a named public ETS table                   |
%%% | `named:`      | Look up an existing named table by atom name      |
%%%
%%% ## Instance Methods
%%%
%%% | Selector           | Description                                  |
%%% |--------------------|----------------------------------------------|
%%% | `lookup:key:`      | Look up a key; nil if absent                 |
%%% | `insert:key:value:`| Insert or update a key-value pair            |
%%% | `lookupIfAbsent:key:block:` | Lookup with default block           |
%%% | `includesKey:key:` | Test key membership                          |
%%% | `removeKey:key:`   | Delete an entry; returns nil                 |
%%% | `keys:`            | Return all keys as a List                    |
%%% | `tableSize:`       | Number of entries                            |
%%% | `deleteTable:`     | Destroy the table                            |
%%%
%%% ## References
%%%
%%% - BT-1189: ETS — shared in-memory table class for actor state sharing
%%% - ADR 0042: Actor-Only Mutable State
%%% - ADR 0055: Erlang-Backed Class Protocol

-module(beamtalk_ets).

%% Class methods — canonical colon forms (for EUnit tests and Beamtalk dispatch)
-export(['new:type:'/2, 'named:'/1]).

%% Instance methods — no-colon forms used by the FFI proxy
%% The (Erlang beamtalk_ets) dispatch strips the first keyword from the selector
%% e.g. `lookup: self key: key` → selector `lookup:key:` → function `lookup/2`
-export([lookup/2, insert/3, lookupIfAbsent/3, includesKey/2, removeKey/2]).
-export([keys/1, tableSize/1, deleteTable/1]).

%% FFI shims for class methods: `(Erlang beamtalk_ets) new: name type: t` → `new/2`
-export([new/2, named/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

%% @doc Create a new named public ETS table.
%%
%% `TableType` must be one of: `set`, `orderedSet`, `bag`, `duplicateBag`.
%% The table is created with `named_table` and `public` access so it can be
%% read and written by any process.
%%
%% Raises `already_exists` if a table with the same name already exists.
%% Raises `type_error` if arguments are not atoms.
-spec 'new:type:'(atom(), atom()) -> map().
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

%% @doc Look up an existing named ETS table.
%%
%% Returns an Ets instance if a table with the given name exists.
%% Raises `not_found` if no table with that name has been created.
%% Raises `type_error` if the argument is not an atom.
-spec 'named:'(atom()) -> map().
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

%%% ============================================================================
%%% Instance Methods
%%%
%%% All instance methods receive the Ets tagged map as the first argument.
%%% The FFI proxy calls these by stripping the first keyword from the selector:
%%%   `lookup: self key: key` → selector `lookup:key:` → `lookup(Self, Key)`
%%% ============================================================================

%% @doc Look up a key. Returns the value, or nil if the key is absent.
-spec lookup(map(), term()) -> term().
lookup(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    case ets:lookup(TableName, Key) of
        [{_K, Value}] -> Value;
        [] -> nil
    end;
lookup(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Insert or update a key-value pair. Returns nil.
-spec insert(map(), term(), term()) -> nil.
insert(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Value) ->
    ets:insert(TableName, {Key, Value}),
    nil;
insert(_Self, _Key, _Value) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Look up a key; if absent, evaluate the block and return its result.
-spec lookupIfAbsent(map(), term(), function()) -> term().
lookupIfAbsent(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Block) when
    is_function(Block, 0)
->
    case ets:lookup(TableName, Key) of
        [{_K, Value}] -> Value;
        [] -> Block()
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

%% @doc Test whether a key exists in the table.
-spec includesKey(map(), term()) -> boolean().
includesKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    ets:member(TableName, Key);
includesKey(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'includesKey:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Remove the entry for key. Returns nil.
-spec removeKey(map(), term()) -> nil.
removeKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    ets:delete(TableName, Key),
    nil;
removeKey(_Self, _Key) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'removeKey:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Return all keys in the table as a List.
-spec keys(map()) -> list().
keys(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    ets:select(TableName, [{{'$1', '_'}, [], ['$1']}]);
keys(_Self) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'keys'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Return the number of entries in the table.
%%
%% Named `tableSize` to avoid shadowing the deprecated `erlang:size/1` BIF.
-spec tableSize(map()) -> non_neg_integer().
tableSize(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    ets:info(TableName, size);
tableSize(_Self) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'size'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be an Ets instance">>),
    beamtalk_error:raise(Error2).

%% @doc Destroy the ETS table. Returns nil.
%%
%% Named `deleteTable` to avoid shadowing `ets:delete/1` in call sites.
-spec deleteTable(map()) -> nil.
deleteTable(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    ets:delete(TableName),
    nil;
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

%% @doc FFI shim for new:type: — called via (Erlang beamtalk_ets) new: name type: type.
-spec new(atom(), atom()) -> map().
new(Name, TableType) -> 'new:type:'(Name, TableType).

%% @doc FFI shim for named: — called via (Erlang beamtalk_ets) named: name.
-spec named(atom()) -> map().
named(Name) -> 'named:'(Name).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private
%% @doc Build an Ets tagged map for the given table name.
-spec make_ets(atom()) -> map().
make_ets(Name) ->
    #{'$beamtalk_class' => 'Ets', table => Name}.

%% @private
%% @doc Map Beamtalk table type symbols to OTP ets type atoms.
%%
%% Beamtalk uses camelCase symbols matching the Smalltalk convention.
%% OTP uses snake_case atoms.
-spec map_table_type(atom()) -> set | ordered_set | bag | duplicate_bag.
map_table_type(set) ->
    set;
map_table_type(orderedSet) ->
    ordered_set;
map_table_type(bag) ->
    bag;
map_table_type(duplicateBag) ->
    duplicate_bag;
map_table_type(Other) ->
    Error0 = beamtalk_error:new(type_error, 'Ets'),
    Error1 = beamtalk_error:with_selector(Error0, 'new:type:'),
    Error2 = beamtalk_error:with_details(Error1, #{type => Other}),
    Error3 = beamtalk_error:with_hint(
        Error2,
        <<"Table type must be one of: #set, #orderedSet, #bag, #duplicateBag">>
    ),
    beamtalk_error:raise(Error3).
