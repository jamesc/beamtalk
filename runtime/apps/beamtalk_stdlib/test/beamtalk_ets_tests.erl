%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_ets module (BT-1189).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover:
%%% - new:type:/2 — table creation (all four types, type errors, duplicate name)
%%% - named:/1 — named table lookup (success, not_found, type error)
%%% - lookup/2 — key lookup (present, absent, type error)
%%% - insert/3 — insert and update (success, type error)
%%% - lookupIfAbsent/3 — lookup with default block
%%% - includesKey/2 — membership test
%%% - removeKey/2 — key deletion
%%% - keys/1 — all keys as list
%%% - tableSize/1 — entry count
%%% - deleteTable/1 — table destruction
%%% - FFI shims: new/2, named/1

-module(beamtalk_ets_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

%% @doc Create a fresh table with a unique name, run Fun(Table), then clean up.
with_table(Name, Fun) ->
    %% Delete any leftover table from a previous failed run
    catch ets:delete(Name),
    Table = beamtalk_ets:'new:type:'(Name, set),
    try
        Fun(Table)
    after
        catch ets:delete(Name)
    end.

%%% ============================================================================
%%% new:type:/2
%%% ============================================================================

new_set_test() ->
    catch ets:delete(bt_ets_test_new_set),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_new_set, set),
    ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_new_set}, Table),
    ets:delete(bt_ets_test_new_set).

new_ordered_set_test() ->
    catch ets:delete(bt_ets_test_new_ordered),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_new_ordered, orderedSet),
    ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_new_ordered}, Table),
    ets:delete(bt_ets_test_new_ordered).

new_bag_test() ->
    catch ets:delete(bt_ets_test_new_bag),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_new_bag, bag),
    ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_new_bag}, Table),
    ets:delete(bt_ets_test_new_bag).

new_duplicate_bag_test() ->
    catch ets:delete(bt_ets_test_new_dupbag),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_new_dupbag, duplicateBag),
    ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_new_dupbag}, Table),
    ets:delete(bt_ets_test_new_dupbag).

new_already_exists_test() ->
    catch ets:delete(bt_ets_test_dup),
    ets:new(bt_ets_test_dup, [set, named_table, public]),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = already_exists}},
            beamtalk_ets:'new:type:'(bt_ets_test_dup, set)
        )
    after
        ets:delete(bt_ets_test_dup)
    end.

new_type_error_name_not_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:'new:type:'(<<"not_an_atom">>, set)
    ).

new_type_error_type_not_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:'new:type:'(some_table, <<"set">>)
    ).

new_type_error_invalid_type_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:'new:type:'(some_table, invalid_type)
    ).

%%% ============================================================================
%%% named:/1
%%% ============================================================================

named_success_test() ->
    catch ets:delete(bt_ets_test_named),
    ets:new(bt_ets_test_named, [set, named_table, public]),
    try
        Table = beamtalk_ets:'named:'(bt_ets_test_named),
        ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_named}, Table)
    after
        ets:delete(bt_ets_test_named)
    end.

named_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = not_found, class = 'Ets'}},
        beamtalk_ets:'named:'(bt_ets_test_no_such_table_xyz)
    ).

named_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:'named:'(<<"not_an_atom">>)
    ).

%%% ============================================================================
%%% lookup/2
%%% ============================================================================

lookup_present_test() ->
    with_table(bt_ets_test_lookup, fun(Table) ->
        ets:insert(bt_ets_test_lookup, {<<"key">>, <<"value">>}),
        ?assertEqual(<<"value">>, beamtalk_ets:lookup(Table, <<"key">>))
    end).

lookup_absent_returns_nil_test() ->
    with_table(bt_ets_test_lookup_absent, fun(Table) ->
        ?assertEqual(nil, beamtalk_ets:lookup(Table, <<"missing">>))
    end).

lookup_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:lookup(not_an_ets, <<"key">>)
    ).

%%% ============================================================================
%%% insert/3
%%% ============================================================================

insert_success_test() ->
    with_table(bt_ets_test_insert, fun(Table) ->
        ?assertEqual(nil, beamtalk_ets:insert(Table, <<"key">>, <<"val">>)),
        ?assertEqual([{<<"key">>, <<"val">>}], ets:lookup(bt_ets_test_insert, <<"key">>))
    end).

insert_update_test() ->
    with_table(bt_ets_test_insert_update, fun(Table) ->
        beamtalk_ets:insert(Table, <<"key">>, <<"original">>),
        beamtalk_ets:insert(Table, <<"key">>, <<"updated">>),
        ?assertEqual([{<<"key">>, <<"updated">>}], ets:lookup(bt_ets_test_insert_update, <<"key">>))
    end).

insert_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:insert(not_an_ets, <<"key">>, <<"val">>)
    ).

%%% ============================================================================
%%% lookupIfAbsent/3
%%% ============================================================================

lookupIfAbsent_present_test() ->
    with_table(bt_ets_test_lookup_default, fun(Table) ->
        beamtalk_ets:insert(Table, <<"key">>, 42),
        Result = beamtalk_ets:lookupIfAbsent(Table, <<"key">>, fun() -> 0 end),
        ?assertEqual(42, Result)
    end).

lookupIfAbsent_absent_test() ->
    with_table(bt_ets_test_lookup_default_absent, fun(Table) ->
        Result = beamtalk_ets:lookupIfAbsent(Table, <<"missing">>, fun() -> 99 end),
        ?assertEqual(99, Result)
    end).

lookupIfAbsent_type_error_not_block_test() ->
    with_table(bt_ets_test_lookup_default_type, fun(Table) ->
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
            beamtalk_ets:lookupIfAbsent(Table, <<"key">>, not_a_function)
        )
    end).

lookupIfAbsent_type_error_not_ets_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:lookupIfAbsent(not_an_ets, <<"key">>, fun() -> 0 end)
    ).

%%% ============================================================================
%%% includesKey/2
%%% ============================================================================

includesKey_present_test() ->
    with_table(bt_ets_test_includes, fun(Table) ->
        beamtalk_ets:insert(Table, <<"key">>, <<"val">>),
        ?assert(beamtalk_ets:includesKey(Table, <<"key">>))
    end).

includesKey_absent_test() ->
    with_table(bt_ets_test_includes_absent, fun(Table) ->
        ?assertNot(beamtalk_ets:includesKey(Table, <<"missing">>))
    end).

includesKey_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:includesKey(not_an_ets, <<"key">>)
    ).

%%% ============================================================================
%%% removeKey/2
%%% ============================================================================

removeKey_existing_test() ->
    with_table(bt_ets_test_remove, fun(Table) ->
        beamtalk_ets:insert(Table, <<"key">>, <<"val">>),
        ?assertEqual(nil, beamtalk_ets:removeKey(Table, <<"key">>)),
        ?assertEqual([], ets:lookup(bt_ets_test_remove, <<"key">>))
    end).

removeKey_missing_is_idempotent_test() ->
    with_table(bt_ets_test_remove_missing, fun(Table) ->
        %% Removing a non-existent key does not raise
        ?assertEqual(nil, beamtalk_ets:removeKey(Table, <<"no_such_key">>))
    end).

removeKey_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:removeKey(not_an_ets, <<"key">>)
    ).

%%% ============================================================================
%%% keys/1
%%% ============================================================================

keys_empty_test() ->
    with_table(bt_ets_test_keys_empty, fun(Table) ->
        ?assertEqual([], beamtalk_ets:keys(Table))
    end).

keys_multiple_test() ->
    with_table(bt_ets_test_keys_multi, fun(Table) ->
        beamtalk_ets:insert(Table, <<"a">>, 1),
        beamtalk_ets:insert(Table, <<"b">>, 2),
        beamtalk_ets:insert(Table, <<"c">>, 3),
        Keys = beamtalk_ets:keys(Table),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], lists:sort(Keys))
    end).

keys_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:keys(not_an_ets)
    ).

%%% ============================================================================
%%% tableSize/1
%%% ============================================================================

tableSize_empty_test() ->
    with_table(bt_ets_test_size_empty, fun(Table) ->
        ?assertEqual(0, beamtalk_ets:tableSize(Table))
    end).

tableSize_after_insert_test() ->
    with_table(bt_ets_test_size_insert, fun(Table) ->
        beamtalk_ets:insert(Table, <<"a">>, 1),
        ?assertEqual(1, beamtalk_ets:tableSize(Table)),
        beamtalk_ets:insert(Table, <<"b">>, 2),
        ?assertEqual(2, beamtalk_ets:tableSize(Table))
    end).

tableSize_after_update_does_not_increase_test() ->
    with_table(bt_ets_test_size_update, fun(Table) ->
        beamtalk_ets:insert(Table, <<"key">>, 1),
        beamtalk_ets:insert(Table, <<"key">>, 2),
        ?assertEqual(1, beamtalk_ets:tableSize(Table))
    end).

tableSize_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:tableSize(not_an_ets)
    ).

%%% ============================================================================
%%% deleteTable/1
%%% ============================================================================

deleteTable_success_test() ->
    catch ets:delete(bt_ets_test_delete),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_delete, set),
    ?assertEqual(nil, beamtalk_ets:deleteTable(Table)),
    ?assertEqual(undefined, ets:whereis(bt_ets_test_delete)).

deleteTable_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Ets'}},
        beamtalk_ets:deleteTable(not_an_ets)
    ).

%%% ============================================================================
%%% FFI shims: new/2, named/1
%%% ============================================================================

ffi_new_shim_test() ->
    catch ets:delete(bt_ets_test_ffi_new),
    Table = beamtalk_ets:new(bt_ets_test_ffi_new, set),
    ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_ffi_new}, Table),
    ets:delete(bt_ets_test_ffi_new).

ffi_named_shim_success_test() ->
    catch ets:delete(bt_ets_test_ffi_named),
    ets:new(bt_ets_test_ffi_named, [set, named_table, public]),
    try
        Table = beamtalk_ets:named(bt_ets_test_ffi_named),
        ?assertMatch(#{'$beamtalk_class' := 'Ets', table := bt_ets_test_ffi_named}, Table)
    after
        ets:delete(bt_ets_test_ffi_named)
    end.

ffi_named_shim_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = not_found}},
        beamtalk_ets:named(bt_ets_test_no_such_xyz_ffi)
    ).

%%% ============================================================================
%%% Bag / duplicate_bag table semantics
%%% ============================================================================

bag_insert_upsert_test() ->
    %% insert on a bag table replaces existing values for the key (delete-then-insert)
    catch ets:delete(bt_ets_test_bag_upsert),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_bag_upsert, bag),
    try
        beamtalk_ets:insert(Table, <<"key">>, <<"first">>),
        beamtalk_ets:insert(Table, <<"key">>, <<"second">>),
        %% After upsert only one entry should exist
        ?assertEqual([{<<"key">>, <<"second">>}], ets:lookup(bt_ets_test_bag_upsert, <<"key">>)),
        ?assertEqual(1, beamtalk_ets:tableSize(Table))
    after
        catch ets:delete(bt_ets_test_bag_upsert)
    end.

bag_lookup_returns_value_test() ->
    %% lookup/2 returns the stored value for a bag table
    catch ets:delete(bt_ets_test_bag_lookup),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_bag_lookup, bag),
    try
        beamtalk_ets:insert(Table, <<"key">>, <<"value">>),
        ?assertEqual(<<"value">>, beamtalk_ets:lookup(Table, <<"key">>))
    after
        catch ets:delete(bt_ets_test_bag_lookup)
    end.

bag_keys_unique_test() ->
    %% keys/1 returns each key only once even for bag tables
    catch ets:delete(bt_ets_test_bag_keys),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_bag_keys, bag),
    try
        beamtalk_ets:insert(Table, <<"k1">>, <<"a">>),
        beamtalk_ets:insert(Table, <<"k2">>, <<"b">>),
        Keys = beamtalk_ets:keys(Table),
        ?assertEqual([<<"k1">>, <<"k2">>], lists:sort(Keys))
    after
        catch ets:delete(bt_ets_test_bag_keys)
    end.

duplicate_bag_insert_upsert_test() ->
    %% insert on a duplicate_bag table also replaces existing values (delete-then-insert)
    catch ets:delete(bt_ets_test_dupbag_upsert),
    Table = beamtalk_ets:'new:type:'(bt_ets_test_dupbag_upsert, duplicateBag),
    try
        beamtalk_ets:insert(Table, <<"key">>, <<"first">>),
        beamtalk_ets:insert(Table, <<"key">>, <<"second">>),
        ?assertEqual([{<<"key">>, <<"second">>}], ets:lookup(bt_ets_test_dupbag_upsert, <<"key">>)),
        ?assertEqual(1, beamtalk_ets:tableSize(Table))
    after
        catch ets:delete(bt_ets_test_dupbag_upsert)
    end.
