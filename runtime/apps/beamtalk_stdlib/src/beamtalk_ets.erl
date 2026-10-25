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
| `values:`          | All values as a List (snapshot)              |
| `keysAndValuesDo:block:` | Iterate a snapshot of key/value pairs        |
| `asDictionary:`    | Snapshot into an immutable Dictionary        |
| `ifPresent:key:block:` | Evaluate block with the value if present |
| `incrementAt:key:by:` | Atomic integer add (set/orderedSet)       |
| `updateAt:key:block:` | Atomic compare-and-swap update (set/orderedSet) |
| `removeAll:`       | Remove every entry, keep the table           |

## References

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
-export([
    removeAll/1, values/1, keysAndValuesDo/2, asDictionary/1, ifPresent/3, incrementAt/3, updateAt/3
]).

%% `at/2` is a same-named alias for `self delegate` on `at:` to reach —
%% kept separate from `lookup/2` (the FFI-proxy shim name above) purely so
%% the exported name matches the Beamtalk selector.
-export([at/2]).

%% Opt-out of the FFI proxy's ok/error -> Result coercion: Ets returns user data.
-export([beamtalk_ffi_raw_results/0]).

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
-spec 'new:type:'(atom(), Type :: atom()) -> t().
'new:type:'(Name, TableType) when is_atom(Name), is_atom(TableType) ->
    EtsType = map_table_type(TableType),
    try
        ets:new(Name, [EtsType, named_table, public, {keypos, 1}]),
        make_ets(Name)
    catch
        error:badarg ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    already_exists, 'Ets', 'new:type:', <<"A table with this name already exists">>
                )
            )
    end;
'new:type:'(Name, _TableType) when not is_atom(Name) ->
    beamtalk_error:raise_type_error('Ets', 'new:type:', <<"Table name must be a Symbol">>);
'new:type:'(_Name, _TableType) ->
    beamtalk_error:raise_type_error(
        'Ets',
        'new:type:',
        <<"Table type must be a Symbol (#set, #orderedSet, #bag, #duplicateBag)">>
    ).

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
            beamtalk_error:raise(
                beamtalk_error:new(
                    not_found, 'Ets', 'named:', <<"No ETS table with this name exists">>
                )
            );
        _Tid ->
            make_ets(Name)
    end;
'named:'(_) ->
    beamtalk_error:raise_type_error('Ets', 'named:', <<"Table name must be a Symbol">>).

-doc """
Check whether a named ETS table exists.

Returns `true` if a table with the given name exists, `false` otherwise.
Raises `type_error` if the argument is not an atom.
""".
-spec 'exists:'(atom()) -> boolean().
'exists:'(Name) when is_atom(Name) ->
    ets:whereis(Name) =/= undefined;
'exists:'(_) ->
    beamtalk_error:raise_type_error('Ets', 'exists:', <<"Table name must be a Symbol">>).

-doc """
Create a named table or return the existing one.

If a table with the given name already exists, returns an Ets instance
wrapping it. Otherwise creates a new table with the specified type.
Raises `type_error` if arguments are not atoms.
""".
-spec 'newOrExisting:type:'(atom(), Type :: atom()) -> t().
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
                    beamtalk_error:raise(
                        beamtalk_error:new(
                            already_exists,
                            'Ets',
                            'newOrExisting:type:',
                            <<"Failed to create or find table">>
                        )
                    );
                _Tid ->
                    make_ets(Name)
            end
    end;
'newOrExisting:type:'(Name, _TableType) when not is_atom(Name) ->
    beamtalk_error:raise_type_error(
        'Ets', 'newOrExisting:type:', <<"Table name must be a Symbol">>
    );
'newOrExisting:type:'(_Name, _TableType) ->
    beamtalk_error:raise_type_error(
        'Ets',
        'newOrExisting:type:',
        <<"Table type must be a Symbol (#set, #orderedSet, #bag, #duplicateBag)">>
    ).

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
-spec lookup(t(), Key :: term()) -> term().
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
    beamtalk_error:raise_type_error('Ets', 'at:', <<"Receiver must be an Ets instance">>).

-spec at(t(), Key :: term()) -> term().
at(Self, Key) -> lookup(Self, Key).

-doc """
Insert or update a key-value pair. Returns nil.

For bag/duplicate_bag tables, deletes all existing entries for Key before
inserting, giving best-effort upsert semantics. Note: the delete and insert
are two separate ETS operations. Concurrent writes from multiple actors to
the same key on a bag table are not serialized — interleaved calls may
result in multiple entries for the same key.
""".
-spec insert(t(), Key :: term(), Value :: term()) -> nil.
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
    beamtalk_error:raise_type_error('Ets', 'at:put:', <<"Receiver must be an Ets instance">>).

-doc "Look up a key; if absent, evaluate the block and return its result.".
-spec lookupIfAbsent(t(), Key :: term(), Block :: function()) -> term().
lookupIfAbsent(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Block) when
    is_function(Block, 0)
->
    case guarded(TableName, 'at:ifAbsent:', fun() -> ets:lookup(TableName, Key) end) of
        [{_K, Value} | _] -> Value;
        [] -> call_block(Block, [], 'at:ifAbsent:')
    end;
lookupIfAbsent(#{'$beamtalk_class' := 'Ets'}, _Key, _Block) ->
    beamtalk_error:raise_type_error('Ets', 'at:ifAbsent:', <<"Third argument must be a Block">>);
lookupIfAbsent(_Self, _Key, _Block) ->
    beamtalk_error:raise_type_error('Ets', 'at:ifAbsent:', <<"Receiver must be an Ets instance">>).

-doc "Test whether a key exists in the table.".
-spec includesKey(t(), Key :: term()) -> boolean().
includesKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    try
        ets:member(TableName, Key)
    catch
        error:badarg -> stale_table_error('includesKey:key:', TableName)
    end;
includesKey(_Self, _Key) ->
    beamtalk_error:raise_type_error('Ets', 'includesKey:', <<"Receiver must be an Ets instance">>).

-doc "Remove the entry for key. Returns nil.".
-spec removeKey(t(), Key :: term()) -> nil.
removeKey(#{'$beamtalk_class' := 'Ets', table := TableName}, Key) ->
    try
        ets:delete(TableName, Key),
        nil
    catch
        error:badarg -> stale_table_error('removeKey:key:', TableName)
    end;
removeKey(_Self, _Key) ->
    beamtalk_error:raise_type_error('Ets', 'removeKey:', <<"Receiver must be an Ets instance">>).

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
    beamtalk_error:raise_type_error('Ets', 'keys', <<"Receiver must be an Ets instance">>).

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
    beamtalk_error:raise_type_error('Ets', 'size', <<"Receiver must be an Ets instance">>).

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
            beamtalk_error:raise(
                beamtalk_error:new(
                    permission_error,
                    'Ets',
                    'delete',
                    <<"Caller is not the owner of the ETS table, or the table has already been deleted">>
                )
            )
    end;
deleteTable(_Self) ->
    beamtalk_error:raise_type_error('Ets', 'delete', <<"Receiver must be an Ets instance">>).

-doc """
Marker read by `beamtalk_erlang_proxy:coerce_ffi_result/2`: results of this
module are user data and must reach Beamtalk unchanged (a stored `#ok` stays
`#ok`, not `Result ok: nil`). No function here signals failure through an
`ok`/`{error, _}` return, so nothing relies on the coercion.
""".
-spec beamtalk_ffi_raw_results() -> true.
beamtalk_ffi_raw_results() -> true.

%%% ============================================================================
%%% Bulk Reads, Conditional Lookup and Atomic Updates (BT-3831)
%%% ============================================================================

-define(MAX_UPDATE_RETRIES, 1000).

-doc "Remove every entry, keeping the table. Returns nil.".
-spec removeAll(t()) -> nil.
removeAll(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    try
        ets:delete_all_objects(TableName),
        nil
    catch
        error:badarg -> stale_table_error('removeAll', TableName)
    end;
removeAll(_Self) ->
    beamtalk_error:raise_type_error('Ets', 'removeAll', <<"Receiver must be an Ets instance">>).

-doc """
Return all values as a List. A point-in-time `ets:tab2list/1` read: order is
unspecified for set/bag tables and key order for orderedSet. Bag tables yield
one value per stored entry.
""".
-spec values(t()) -> list().
values(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    [V || {_K, V} <- entries(TableName, 'values')];
values(_Self) ->
    beamtalk_error:raise_type_error('Ets', 'values', <<"Receiver must be an Ets instance">>).

-doc """
Evaluate Block(Key, Value) for every entry of a snapshot taken up front, so
the block may freely modify the table. Returns nil.
""".
-spec keysAndValuesDo(t(), function()) -> nil.
keysAndValuesDo(#{'$beamtalk_class' := 'Ets', table := TableName}, Block) when
    is_function(Block, 2)
->
    lists:foreach(
        fun({K, V}) -> call_block(Block, [K, V], 'keysAndValuesDo:') end,
        entries(TableName, 'keysAndValuesDo:')
    ),
    nil;
keysAndValuesDo(#{'$beamtalk_class' := 'Ets'}, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'keysAndValuesDo:', <<"Argument must be a two-argument Block">>
    );
keysAndValuesDo(_Self, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'keysAndValuesDo:', <<"Receiver must be an Ets instance">>
    ).

-doc """
Snapshot the table into an immutable Dictionary (an Erlang map). For bag
tables, the last entry read for a key wins.
""".
-spec asDictionary(t()) -> map().
asDictionary(#{'$beamtalk_class' := 'Ets', table := TableName}) ->
    maps:from_list(entries(TableName, 'asDictionary'));
asDictionary(_Self) ->
    beamtalk_error:raise_type_error(
        'Ets', 'asDictionary', <<"Receiver must be an Ets instance">>
    ).

-doc "If Key is present evaluate Block(Value) and answer its result, else nil.".
-spec ifPresent(t(), Key :: term(), function()) -> term().
ifPresent(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Block) when
    is_function(Block, 1)
->
    case guarded(TableName, 'at:ifPresent:', fun() -> ets:lookup(TableName, Key) end) of
        [{_K, Value} | _] -> call_block(Block, [Value], 'at:ifPresent:');
        [] -> nil
    end;
ifPresent(#{'$beamtalk_class' := 'Ets'}, _Key, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:ifPresent:', <<"Third argument must be a one-argument Block">>
    );
ifPresent(_Self, _Key, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:ifPresent:', <<"Receiver must be an Ets instance">>
    ).

-doc """
Atomically add N to the integer stored at Key (missing key starts at 0) via
`ets:update_counter/4`. Answers the new value. set/orderedSet only.
""".
-spec incrementAt(t(), Key :: term(), integer()) -> integer().
incrementAt(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, N) when is_integer(N) ->
    require_unique_keys(TableName, 'at:increment:'),
    guarded(
        TableName,
        'at:increment:',
        fun() -> ets:update_counter(TableName, Key, {2, N}, {Key, 0}) end,
        <<"Stored value must be an Integer">>
    );
incrementAt(#{'$beamtalk_class' := 'Ets'}, _Key, _N) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:increment:', <<"Increment must be an Integer">>
    );
incrementAt(_Self, _Key, _N) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:increment:', <<"Receiver must be an Ets instance">>
    ).

-doc """
Atomic read-modify-write via an optimistic compare-and-swap loop. Block gets
the current value (nil if missing) and answers the new value, which is stored
and answered. Block may run more than once under contention and must be free
of side effects. A raising Block propagates and writes nothing. Retries are
bounded; exhaustion raises `update_contention`. set/orderedSet only.
""".
-spec updateAt(t(), Key :: term(), function()) -> term().
updateAt(#{'$beamtalk_class' := 'Ets', table := TableName}, Key, Block) when
    is_function(Block, 1)
->
    require_unique_keys(TableName, 'at:update:'),
    require_plain_key(Key, 'at:update:'),
    update_loop(TableName, Key, Block, ?MAX_UPDATE_RETRIES);
updateAt(#{'$beamtalk_class' := 'Ets'}, _Key, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:update:', <<"Second argument must be a one-argument Block">>
    );
updateAt(_Self, _Key, _Block) ->
    beamtalk_error:raise_type_error(
        'Ets', 'at:update:', <<"Receiver must be an Ets instance">>
    ).

-doc """
Yield the scheduler before retrying a lost compare-and-swap, so a writer that
keeps losing on a hot key gives the winners room to finish. The retry bound is
unchanged.
""".
-spec retry_update(atom(), term(), function(), pos_integer()) -> term().
retry_update(TableName, Key, Block, Retries) ->
    erlang:yield(),
    update_loop(TableName, Key, Block, Retries - 1).

-spec update_loop(atom(), term(), function(), non_neg_integer()) -> term().
update_loop(TableName, Key, _Block, 0) ->
    beamtalk_error:raise(
        beamtalk_error:with_details(
            beamtalk_error:new(
                update_contention,
                'Ets',
                'at:update:',
                <<"Gave up after repeated conflicting concurrent updates">>
            ),
            #{table => TableName, key => Key}
        )
    );
update_loop(TableName, Key, Block, Retries) ->
    case guarded(TableName, 'at:update:', fun() -> ets:lookup(TableName, Key) end) of
        [] ->
            New = call_block(Block, [nil], 'at:update:'),
            %% insert_new fails if another writer created the key meanwhile.
            case
                guarded(TableName, 'at:update:', fun() -> ets:insert_new(TableName, {Key, New}) end)
            of
                true -> New;
                false -> retry_update(TableName, Key, Block, Retries)
            end;
        [{StoredKey, Old}] ->
            New = call_block(Block, [Old], 'at:update:'),
            %% Match the key as stored (an orderedSet may hold 1 for lookup 1.0)
            %% and the exact old value, so a concurrent change makes the
            %% replace a no-op.
            MS = [
                {
                    {StoredKey, '$1'},
                    [{'=:=', '$1', {const, Old}}],
                    [{{{const, StoredKey}, {const, New}}}]
                }
            ],
            case guarded(TableName, 'at:update:', fun() -> ets:select_replace(TableName, MS) end) of
                1 -> New;
                0 -> retry_update(TableName, Key, Block, Retries)
            end
    end.

-doc """
Run an ETS operation, mapping a `badarg` to a stale-table error when the table
is gone. Any other `badarg` becomes a structured `type_error`, so a raw
`badarg` never escapes to the FFI proxy's charlist retry (which would re-run
the operation under a different key).
""".
-spec guarded(atom(), atom(), fun(() -> term())) -> term().
guarded(TableName, Selector, Fun) ->
    guarded(TableName, Selector, Fun, <<"ETS operation rejected its arguments">>).

-doc "As `guarded/3`, with the message used for a `type_error` when the table is not stale.".
-spec guarded(atom(), atom(), fun(() -> term()), binary()) -> term().
guarded(TableName, Selector, Fun, BadArgMessage) ->
    %% bt-catcher-audit: not-applicable-no-block - Fun is always an internal ETS operation
    %% closure built in this module, never a Beamtalk block.
    try
        Fun()
    catch
        error:badarg ->
            case ets:whereis(TableName) of
                undefined ->
                    stale_table_error(Selector, TableName);
                _ ->
                    beamtalk_error:raise_type_error('Ets', Selector, BadArgMessage)
            end
    end.

-doc """
Call a user Block with Args, converting a raw `badarg` into a structured error
that keeps the original reason and stacktrace in its details.
""".
-spec call_block(function(), [term()], atom()) -> term().
call_block(Block, Args, Selector) ->
    %% bt-catcher-audit: not-applicable-reraises - the only catch clause (error:badarg) re-raises
    %% a structured #beamtalk_error{}; the next boundary restores class variables.
    try
        apply(Block, Args)
    catch
        error:badarg:St ->
            beamtalk_error:raise(
                beamtalk_error:with_details(
                    beamtalk_error:new(
                        type_error, 'Ets', Selector, <<"Block raised badarg">>
                    ),
                    #{reason => badarg, stacktrace => St}
                )
            )
    end.

-doc """
Raise a structured error if the key contains an atom ETS match specs read as a
pattern variable (`'_'` or `'$N'`) or a map (matched as a subset), since the CAS embeds the key in a match head.
""".
-spec require_plain_key(term(), atom()) -> ok.
require_plain_key(Key, Selector) ->
    case has_match_variable(Key) of
        false ->
            ok;
        true ->
            beamtalk_error:raise_type_error(
                'Ets',
                Selector,
                <<"Keys containing #_ or #$N symbols, or Dictionaries, are not supported">>
            )
    end.

-spec has_match_variable(term()) -> boolean().
has_match_variable('_') ->
    true;
has_match_variable(A) when is_atom(A) ->
    case atom_to_list(A) of
        [$$ | Digits] ->
            Digits =/= [] andalso lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Digits);
        _ ->
            false
    end;
has_match_variable(T) when is_tuple(T) -> has_match_variable(tuple_to_list(T));
has_match_variable([H | T]) ->
    has_match_variable(H) orelse has_match_variable(T);
%% A map in a match head matches any superset map, so it is not an exact key.
has_match_variable(M) when is_map(M) -> true;
has_match_variable(_) ->
    false.

-doc "Raise a structured error unless the table holds one entry per key.".
-spec require_unique_keys(atom(), atom()) -> ok.
require_unique_keys(TableName, Selector) ->
    case ets:info(TableName, type) of
        T when T =:= set; T =:= ordered_set ->
            ok;
        undefined ->
            stale_table_error(Selector, TableName);
        Other ->
            beamtalk_error:raise(
                beamtalk_error:with_details(
                    beamtalk_error:new(
                        unsupported_table_type,
                        'Ets',
                        Selector,
                        <<"Atomic updates are supported on #set and #orderedSet tables only">>
                    ),
                    #{table_type => Other}
                )
            )
    end.

-spec entries(atom(), atom()) -> [{term(), term()}].
entries(TableName, Selector) ->
    guarded(TableName, Selector, fun() -> ets:tab2list(TableName) end).

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_ets) FFI uses beamtalk_erlang_proxy:direct_call/4,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector. These shims provide the colon-free entry points:
%%%   `(Erlang beamtalk_ets) new: name type: t` → `new(name, t)` → `'new:type:'`
%%%   `(Erlang beamtalk_ets) named: name`       → `named(name)` → `'named:'`
%%% ============================================================================

-doc "FFI shim for new:type: — called via (Erlang beamtalk_ets) new: name type: type.".
-spec new(atom(), Type :: atom()) -> t().
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
-spec newOrExisting(atom(), Type :: atom()) -> t().
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
    beamtalk_error:raise(
        beamtalk_error:new(
            stale_table,
            'Ets',
            Selector,
            iolist_to_binary([
                <<"ETS table is stale or has been deleted: ">>,
                atom_to_binary(TableName, utf8)
            ])
        )
    ).

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
