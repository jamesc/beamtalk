%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context
%%%
-module(beamtalk_class_methods_table_tests).

-moduledoc """
Unit tests for beamtalk_class_methods_table (BT-2008).

Tests cover:
- new/0 is idempotent
- insert/3 and lookup/1 round-trip with module + selectors
- delete/1 removes the entry
- lookup/1 returns not_found for unknown class
- insert/3 overwrites on hot-reload (module and/or selectors change)
- lookup/1 returns not_found when table is absent
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup helpers
%%====================================================================

%% Save and clear the table before each test; restore after, so real
%% class entries populated during startup are preserved.
setup() ->
    beamtalk_class_methods_table:new(),
    Saved = ets:tab2list(beamtalk_class_methods),
    ets:delete_all_objects(beamtalk_class_methods),
    Saved.

cleanup(Saved) ->
    ets:delete_all_objects(beamtalk_class_methods),
    ets:insert(beamtalk_class_methods, Saved).

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
    ok = beamtalk_class_methods_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_methods, id)).

new_is_idempotent_test() ->
    ok = beamtalk_class_methods_table:new(),
    ok = beamtalk_class_methods_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_methods, id)).

%%====================================================================
%% Tests: insert/3 and lookup/1
%%====================================================================

insert_and_lookup_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_methods_table:insert('MyClass', my_class_module, ['foo', 'bar:']),
        ?assertEqual(
            {ok, my_class_module, ['foo', 'bar:']},
            beamtalk_class_methods_table:lookup('MyClass')
        )
    end).

insert_empty_selector_list_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_methods_table:insert('LeafClass', leaf_module, []),
        ?assertEqual(
            {ok, leaf_module, []},
            beamtalk_class_methods_table:lookup('LeafClass')
        )
    end).

lookup_unknown_returns_not_found_test() ->
    with_clean_table(fun() ->
        ?assertEqual(not_found, beamtalk_class_methods_table:lookup('NonExistentClass'))
    end).

insert_overwrites_on_hot_reload_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_methods_table:insert('HotClass', old_module, ['oldSel']),
        ok = beamtalk_class_methods_table:insert('HotClass', new_module, ['newSel', 'newSel2']),
        ?assertEqual(
            {ok, new_module, ['newSel', 'newSel2']},
            beamtalk_class_methods_table:lookup('HotClass')
        )
    end).

multiple_classes_are_independent_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_methods_table:insert('ClassA', mod_a, ['a1', 'a2']),
        ok = beamtalk_class_methods_table:insert('ClassB', mod_b, ['b1']),
        ?assertEqual({ok, mod_a, ['a1', 'a2']}, beamtalk_class_methods_table:lookup('ClassA')),
        ?assertEqual({ok, mod_b, ['b1']}, beamtalk_class_methods_table:lookup('ClassB'))
    end).

%%====================================================================
%% Tests: delete/1
%%====================================================================

delete_removes_entry_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_methods_table:insert('DeleteMe', delete_me_mod, ['x']),
        ok = beamtalk_class_methods_table:delete('DeleteMe'),
        ?assertEqual(not_found, beamtalk_class_methods_table:lookup('DeleteMe'))
    end).

delete_nonexistent_is_safe_test() ->
    with_clean_table(fun() ->
        ?assertEqual(ok, beamtalk_class_methods_table:delete('NeverInserted'))
    end).

%%====================================================================
%% Tests: lookup/1 with absent table
%%====================================================================

lookup_when_table_absent_returns_not_found_test() ->
    %% Save real contents so we can restore the table afterward.
    Saved = ets:tab2list(beamtalk_class_methods),
    (try
        ets:delete(beamtalk_class_methods)
    catch
        _:_ -> ok
    end),
    try
        %% Must not crash when the table does not exist.
        ?assertEqual(not_found, beamtalk_class_methods_table:lookup('AnyClass'))
    after
        %% Recreate and restore so subsequent tests are not affected.
        beamtalk_class_methods_table:new(),
        ets:insert(beamtalk_class_methods, Saved)
    end.
