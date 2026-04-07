%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context
%%%
-module(beamtalk_class_module_table_tests).

-moduledoc """
Unit tests for beamtalk_class_module_table.

Tests cover:
- new/0 is idempotent
- insert/2 and lookup/1 round-trip
- delete/1 removes the entry
- lookup/1 returns not_found for unknown class
- insert/2 overwrites on hot-reload
- lookup/1 returns not_found when table is absent
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup helpers
%%====================================================================

%% Save and clear the table before each test; restore after.
%% Uses the same save/restore pattern as beamtalk_class_hierarchy_table_tests
%% so that real class entries populated during startup are preserved.
setup() ->
    beamtalk_class_module_table:new(),
    Saved = ets:tab2list(beamtalk_class_module),
    ets:delete_all_objects(beamtalk_class_module),
    Saved.

cleanup(Saved) ->
    ets:delete_all_objects(beamtalk_class_module),
    ets:insert(beamtalk_class_module, Saved).

with_clean_table(Fun) ->
    Saved = setup(),
    try
        Fun()
    after
        cleanup(Saved)
    end.

%%====================================================================
%% Tests: new/0
%%====================================================================

new_creates_table_test() ->
    ok = beamtalk_class_module_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module, id)).

new_is_idempotent_test() ->
    ok = beamtalk_class_module_table:new(),
    ok = beamtalk_class_module_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module, id)).

%%====================================================================
%% Tests: insert/2 and lookup/1
%%====================================================================

insert_and_lookup_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_module_table:insert('MyClass', my_class_module),
        ?assertEqual({ok, my_class_module}, beamtalk_class_module_table:lookup('MyClass'))
    end).

lookup_unknown_returns_not_found_test() ->
    with_clean_table(fun() ->
        ?assertEqual(not_found, beamtalk_class_module_table:lookup('NonExistentClass'))
    end).

insert_overwrites_on_hot_reload_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_module_table:insert('HotClass', old_module),
        ok = beamtalk_class_module_table:insert('HotClass', new_module),
        ?assertEqual({ok, new_module}, beamtalk_class_module_table:lookup('HotClass'))
    end).

multiple_classes_are_independent_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_module_table:insert('ClassA', mod_a),
        ok = beamtalk_class_module_table:insert('ClassB', mod_b),
        ?assertEqual({ok, mod_a}, beamtalk_class_module_table:lookup('ClassA')),
        ?assertEqual({ok, mod_b}, beamtalk_class_module_table:lookup('ClassB'))
    end).

%%====================================================================
%% Tests: delete/1
%%====================================================================

delete_removes_entry_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_module_table:insert('DeleteMe', delete_me_mod),
        ok = beamtalk_class_module_table:delete('DeleteMe'),
        ?assertEqual(not_found, beamtalk_class_module_table:lookup('DeleteMe'))
    end).

delete_nonexistent_is_safe_test() ->
    with_clean_table(fun() ->
        ?assertEqual(ok, beamtalk_class_module_table:delete('NeverInserted'))
    end).

%%====================================================================
%% Tests: lookup/1 with absent table
%%====================================================================

lookup_when_table_absent_returns_not_found_test() ->
    %% Save real contents so we can restore the table afterward.
    Saved = ets:tab2list(beamtalk_class_module),
    catch ets:delete(beamtalk_class_module),
    try
        %% Must not crash when the table does not exist.
        ?assertEqual(not_found, beamtalk_class_module_table:lookup('AnyClass'))
    after
        %% Recreate and restore so subsequent tests are not affected.
        beamtalk_class_module_table:new(),
        ets:insert(beamtalk_class_module, Saved)
    end.
