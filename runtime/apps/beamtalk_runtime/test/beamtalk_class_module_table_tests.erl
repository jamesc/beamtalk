%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context
%%%
%%% @doc Unit tests for beamtalk_class_module_table.
%%%
%%% Tests cover:
%%% - new/0 is idempotent
%%% - insert/2 and lookup/1 round-trip
%%% - delete/1 removes the entry
%%% - lookup/1 returns not_found for unknown class
%%% - insert/2 overwrites on hot-reload
%%% - lookup/1 returns not_found when table is absent
-module(beamtalk_class_module_table_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup helpers
%%====================================================================

%% Clear the table before each test for isolation.
%% Uses delete_all_objects (not ets:delete) to preserve table ownership,
%% matching the pattern in beamtalk_class_hierarchy_table_tests.
fresh_table() ->
    beamtalk_class_module_table:new(),
    ets:delete_all_objects(beamtalk_class_module).

%%====================================================================
%% Tests: new/0
%%====================================================================

new_creates_table_test() ->
    catch ets:delete(beamtalk_class_module),
    ok = beamtalk_class_module_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module, id)).

new_is_idempotent_test() ->
    catch ets:delete(beamtalk_class_module),
    ok = beamtalk_class_module_table:new(),
    ok = beamtalk_class_module_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module, id)).

%%====================================================================
%% Tests: insert/2 and lookup/1
%%====================================================================

insert_and_lookup_test() ->
    fresh_table(),
    ok = beamtalk_class_module_table:insert('MyClass', my_class_module),
    ?assertEqual({ok, my_class_module}, beamtalk_class_module_table:lookup('MyClass')).

lookup_unknown_returns_not_found_test() ->
    fresh_table(),
    ?assertEqual(not_found, beamtalk_class_module_table:lookup('NonExistentClass')).

insert_overwrites_on_hot_reload_test() ->
    fresh_table(),
    ok = beamtalk_class_module_table:insert('HotClass', old_module),
    ok = beamtalk_class_module_table:insert('HotClass', new_module),
    ?assertEqual({ok, new_module}, beamtalk_class_module_table:lookup('HotClass')).

multiple_classes_are_independent_test() ->
    fresh_table(),
    ok = beamtalk_class_module_table:insert('ClassA', mod_a),
    ok = beamtalk_class_module_table:insert('ClassB', mod_b),
    ?assertEqual({ok, mod_a}, beamtalk_class_module_table:lookup('ClassA')),
    ?assertEqual({ok, mod_b}, beamtalk_class_module_table:lookup('ClassB')).

%%====================================================================
%% Tests: delete/1
%%====================================================================

delete_removes_entry_test() ->
    fresh_table(),
    ok = beamtalk_class_module_table:insert('DeleteMe', delete_me_mod),
    ok = beamtalk_class_module_table:delete('DeleteMe'),
    ?assertEqual(not_found, beamtalk_class_module_table:lookup('DeleteMe')).

delete_nonexistent_is_safe_test() ->
    fresh_table(),
    ?assertEqual(ok, beamtalk_class_module_table:delete('NeverInserted')).

%%====================================================================
%% Tests: lookup/1 with absent table
%%====================================================================

lookup_when_table_absent_returns_not_found_test() ->
    catch ets:delete(beamtalk_class_module),
    %% Must not crash when the table does not exist.
    ?assertEqual(not_found, beamtalk_class_module_table:lookup('AnyClass')).
