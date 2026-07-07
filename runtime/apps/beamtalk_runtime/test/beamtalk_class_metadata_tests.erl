%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_metadata_tests).

%% BT-2222: Unified class metadata table tests. Replaces the former
%% beamtalk_class_module_table_tests, beamtalk_class_methods_table_tests, and
%% beamtalk_class_hierarchy_table_tests after the three tables were merged.

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_class_metadata).
%% BT-2266: sibling table holding runtime class-method funs.
-define(FUN_TABLE, beamtalk_class_method_funs).

%% Save/clear/restore the shared table around each test so the live runtime's
%% rows (if any) are not disturbed.
with_clean_table(Fun) ->
    beamtalk_class_metadata:new(),
    Saved = ets:tab2list(?TABLE),
    ets:delete_all_objects(?TABLE),
    try
        Fun()
    after
        ets:delete_all_objects(?TABLE),
        ets:insert(?TABLE, Saved)
    end.

%% BT-2266: clean both the metadata table and the funs sibling table.
with_clean_tables(Fun) ->
    beamtalk_class_metadata:new(),
    SavedMeta = ets:tab2list(?TABLE),
    SavedFuns = ets:tab2list(?FUN_TABLE),
    ets:delete_all_objects(?TABLE),
    ets:delete_all_objects(?FUN_TABLE),
    try
        Fun()
    after
        ets:delete_all_objects(?TABLE),
        ets:delete_all_objects(?FUN_TABLE),
        ets:insert(?TABLE, SavedMeta),
        ets:insert(?FUN_TABLE, SavedFuns)
    end.

%%====================================================================
%% Table lifecycle
%%====================================================================

new_is_idempotent_test() ->
    beamtalk_class_metadata:new(),
    ?assertNotEqual(undefined, ets:info(?TABLE, id)),
    beamtalk_class_metadata:new(),
    ?assertNotEqual(undefined, ets:info(?TABLE, id)).

%%====================================================================
%% Full-row insert + typed lookups
%%====================================================================

insert_and_lookup_all_fields_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Counter', counter_mod, [increment, reset], 'Actor'),
        ?assertEqual({ok, counter_mod}, beamtalk_class_metadata:lookup_module('Counter')),
        ?assertEqual(
            {ok, counter_mod, [increment, reset]},
            beamtalk_class_metadata:lookup_methods('Counter')
        ),
        ?assertEqual({ok, 'Actor'}, beamtalk_class_metadata:lookup_superclass('Counter'))
    end).

lookup_missing_class_test() ->
    with_clean_table(fun() ->
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_module('Nope')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('Nope')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_superclass('Nope'))
    end).

insert_overwrites_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Hot', old_mod, [a], 'Object'),
        ok = beamtalk_class_metadata:insert('Hot', new_mod, [a, b], 'Value'),
        ?assertEqual({ok, new_mod}, beamtalk_class_metadata:lookup_module('Hot')),
        ?assertEqual({ok, new_mod, [a, b]}, beamtalk_class_metadata:lookup_methods('Hot')),
        ?assertEqual({ok, 'Value'}, beamtalk_class_metadata:lookup_superclass('Hot'))
    end).

%%====================================================================
%% Per-field sentinel semantics (`undefined` = unset, behaves like the
%% old per-table absence of the other fields)
%%====================================================================

partial_superclass_only_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Leaf', undefined, undefined, 'Object'),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_module('Leaf')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('Leaf')),
        ?assertEqual({ok, 'Object'}, beamtalk_class_metadata:lookup_superclass('Leaf'))
    end).

partial_module_only_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Reg', some_mod, undefined, undefined),
        ?assertEqual({ok, some_mod}, beamtalk_class_metadata:lookup_module('Reg')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('Reg')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_superclass('Reg'))
    end).

%% `none` is a real root-class superclass, distinct from the `undefined` sentinel.
superclass_none_is_not_unset_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Root', root_mod, [], none),
        ?assertEqual({ok, none}, beamtalk_class_metadata:lookup_superclass('Root'))
    end).

%% An empty selector list is a real value, distinct from `undefined`.
empty_selectors_is_not_unset_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('NoClassMethods', m, [], 'Object'),
        ?assertEqual({ok, m, []}, beamtalk_class_metadata:lookup_methods('NoClassMethods'))
    end).

%% lookup_methods/1 must not return a module() of `undefined`: selectors without a
%% module is an incomplete row, so it reads as not_found (keeps the spec honest).
lookup_methods_requires_module_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('SelectorsOnly', undefined, [foo], 'Object'),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('SelectorsOnly'))
    end).

%%====================================================================
%% Delete
%%====================================================================

delete_removes_whole_row_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Gone', m, [], 'Object'),
        ok = beamtalk_class_metadata:delete('Gone'),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_module('Gone')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('Gone')),
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_superclass('Gone'))
    end).

delete_missing_is_ok_test() ->
    with_clean_table(fun() ->
        ?assertEqual(ok, beamtalk_class_metadata:delete('NeverInserted'))
    end).

%%====================================================================
%% match_subclasses / foldl
%%====================================================================

match_subclasses_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Object', undefined, undefined, none),
        ok = beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object'),
        ok = beamtalk_class_metadata:insert('Counter', undefined, undefined, 'Actor'),
        ok = beamtalk_class_metadata:insert('Timer', undefined, undefined, 'Actor'),
        ?assertEqual(['Actor'], beamtalk_class_metadata:match_subclasses('Object')),
        ?assertEqual([], beamtalk_class_metadata:match_subclasses('Counter')),
        ?assertEqual(
            ['Counter', 'Timer'], lists:sort(beamtalk_class_metadata:match_subclasses('Actor'))
        )
    end).

foldl_collects_entries_with_superclass_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Object', undefined, undefined, none),
        ok = beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object'),
        Result = beamtalk_class_metadata:foldl(fun({C, S}, Acc) -> Acc#{C => S} end, #{}),
        ?assertEqual(#{'Object' => none, 'Actor' => 'Object'}, Result)
    end).

%% Module-only rows (no superclass) are skipped, matching the old hierarchy
%% table which only ever held rows that had a superclass written.
foldl_skips_rows_without_superclass_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object'),
        ok = beamtalk_class_metadata:insert('ModuleOnly', some_mod, undefined, undefined),
        Result = beamtalk_class_metadata:foldl(fun({C, S}, Acc) -> Acc#{C => S} end, #{}),
        ?assertEqual(#{'Actor' => 'Object'}, Result)
    end).

foldl_empty_returns_acc_test() ->
    with_clean_table(fun() ->
        ?assertEqual(sentinel, beamtalk_class_metadata:foldl(fun(_, Acc) -> Acc end, sentinel))
    end).

%%====================================================================
%% Runtime class-method funs + gate flag (BT-2266 / ADR 0084)
%%====================================================================

%% Default: a freshly inserted compiled-class row has the gate flag off.
has_runtime_class_methods_defaults_false_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('CompileOnly', m, [foo], 'Object'),
        ?assertNot(beamtalk_class_metadata:has_runtime_class_methods('CompileOnly')),
        %% Unknown class is also false (no row).
        ?assertNot(beamtalk_class_metadata:has_runtime_class_methods('NeverSeen'))
    end).

%% Round-trip: write a fun, set the gate flag, fetch it back.
put_and_lookup_class_method_fun_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('Tally', m, [], 'Object'),
        Fun = fun(_CS, CV) -> CV end,
        Info = #{block => Fun, arity => 2},
        ok = beamtalk_class_metadata:put_class_method_fun('Tally', bump, Info),
        ok = beamtalk_class_metadata:set_runtime_class_methods('Tally', [bump]),
        ?assert(beamtalk_class_metadata:has_runtime_class_methods('Tally')),
        ?assertEqual({ok, Info}, beamtalk_class_metadata:lookup_class_method_fun('Tally', bump)),
        %% set_runtime_class_methods also published the selector for chain discovery.
        ?assertEqual({ok, m, [bump]}, beamtalk_class_metadata:lookup_methods('Tally'))
    end).

%% The gate: with the flag unset, a present fun is NOT returned (funs table
%% never consulted). This is the compile-time-only skip guarantee.
lookup_class_method_fun_is_gated_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('Gated', m, [], 'Object'),
        Info = #{block => fun(_, CV) -> CV end, arity => 2},
        %% Write the fun directly but leave the gate flag false.
        ok = beamtalk_class_metadata:put_class_method_fun('Gated', bump, Info),
        ?assertNot(beamtalk_class_metadata:has_runtime_class_methods('Gated')),
        ?assertEqual(error, beamtalk_class_metadata:lookup_class_method_fun('Gated', bump))
    end).

%% A missing selector returns error even when the gate is open.
lookup_class_method_fun_missing_selector_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('Tally', m, [], 'Object'),
        Info = #{block => fun(_, CV) -> CV end, arity => 2},
        ok = beamtalk_class_metadata:put_class_method_fun('Tally', bump, Info),
        ok = beamtalk_class_metadata:set_runtime_class_methods('Tally', [bump]),
        ?assertEqual(error, beamtalk_class_metadata:lookup_class_method_fun('Tally', notThere))
    end).

%% delete_class_method_funs/1 removes only the target class's funs.
delete_class_method_funs_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('A', m, [], 'Object'),
        ok = beamtalk_class_metadata:insert('B', m, [], 'Object'),
        InfoA = #{block => fun(_, CV) -> CV end, arity => 2},
        InfoB = #{block => fun(_, CV) -> CV end, arity => 2},
        ok = beamtalk_class_metadata:put_class_method_fun('A', f, InfoA),
        ok = beamtalk_class_metadata:set_runtime_class_methods('A', [f]),
        ok = beamtalk_class_metadata:put_class_method_fun('B', g, InfoB),
        ok = beamtalk_class_metadata:set_runtime_class_methods('B', [g]),
        ok = beamtalk_class_metadata:delete_class_method_funs('A'),
        %% A's fun is gone (flag still set, but the entry was purged).
        ?assertEqual(error, beamtalk_class_metadata:lookup_class_method_fun('A', f)),
        %% B is untouched.
        ?assertEqual({ok, InfoB}, beamtalk_class_metadata:lookup_class_method_fun('B', g))
    end).

%% delete/1 (class teardown) also purges the funs sibling table.
delete_row_purges_funs_test() ->
    with_clean_tables(fun() ->
        ok = beamtalk_class_metadata:insert('Gone', m, [], 'Object'),
        Info = #{block => fun(_, CV) -> CV end, arity => 2},
        ok = beamtalk_class_metadata:put_class_method_fun('Gone', f, Info),
        ok = beamtalk_class_metadata:set_runtime_class_methods('Gone', [f]),
        ok = beamtalk_class_metadata:delete('Gone'),
        ?assertNot(beamtalk_class_metadata:has_runtime_class_methods('Gone')),
        ?assertEqual(error, beamtalk_class_metadata:lookup_class_method_fun('Gone', f))
    end).

%%====================================================================
%% all_builtins
%%====================================================================

all_builtins_includes_core_classes_test() ->
    Builtins = beamtalk_class_metadata:all_builtins(),
    ?assert(lists:member('Object', Builtins)),
    ?assert(lists:member('Actor', Builtins)),
    ?assert(lists:member('Integer', Builtins)).

%%====================================================================
%% Table-absent paths (BT-2222 follow-up)
%%
%% Existing tests use with_clean_table/with_clean_tables which always
%% call new/0 first, ensuring both tables exist before each assertion.
%% These tests exercise the ets:info(Table) =:= undefined branches that
%% are reached when the tables have not yet been initialised — real
%% behaviour during early boot or tests that run before new/0 is called.
%%====================================================================

%% Helper: delete the main metadata table, run Fun, restore state.
%% Used by the three tests below that exercise the "table absent" return
%% values of match_subclasses/1, foldl/2, and lookup_methods/1.
with_no_main_table(Fun) ->
    beamtalk_class_metadata:new(),
    SavedMeta = ets:tab2list(?TABLE),
    SavedFuns = ets:tab2list(?FUN_TABLE),
    ets:delete(?TABLE),
    try
        Fun()
    after
        beamtalk_class_metadata:new(),
        ets:delete_all_objects(?TABLE),
        ets:delete_all_objects(?FUN_TABLE),
        ets:insert(?TABLE, SavedMeta),
        ets:insert(?FUN_TABLE, SavedFuns)
    end.

%% match_subclasses/1 returns [] when the metadata table does not exist.
match_subclasses_when_table_absent_test() ->
    with_no_main_table(fun() ->
        ?assertEqual([], beamtalk_class_metadata:match_subclasses('Object'))
    end).

%% foldl/2 returns the initial accumulator when the metadata table does not exist.
foldl_when_table_absent_test() ->
    with_no_main_table(fun() ->
        Result = beamtalk_class_metadata:foldl(fun({_, _}, Acc) -> Acc end, sentinel),
        ?assertEqual(sentinel, Result)
    end).

%% lookup_methods/1 returns not_found when the metadata table does not exist
%% (exercises the undefined-table branch inside the internal row/1 function).
lookup_methods_when_table_absent_test() ->
    with_no_main_table(fun() ->
        ?assertEqual(not_found, beamtalk_class_metadata:lookup_methods('AnyClass'))
    end).

%% lookup_class_method_fun/2 returns error when the fun table does not exist,
%% even when the class's has_runtime_class_methods gate flag is true.
%% Exercises the ets:info(?FUN_TABLE) =:= undefined branch (line ~409).
lookup_class_method_fun_when_fun_table_absent_test() ->
    beamtalk_class_metadata:new(),
    SavedMeta = ets:tab2list(?TABLE),
    SavedFuns = ets:tab2list(?FUN_TABLE),
    ets:delete_all_objects(?TABLE),
    ets:delete_all_objects(?FUN_TABLE),
    try
        %% Set up a class with the gate flag enabled while both tables exist.
        ok = beamtalk_class_metadata:insert('GateClass', m, [], 'Object'),
        Info = #{block => fun(_, V) -> V end, arity => 2},
        ok = beamtalk_class_metadata:put_class_method_fun('GateClass', f, Info),
        ok = beamtalk_class_metadata:set_runtime_class_methods('GateClass', [f]),
        ?assert(beamtalk_class_metadata:has_runtime_class_methods('GateClass')),
        %% Delete the fun table to simulate it being absent.
        ets:delete(?FUN_TABLE),
        %% Gate is open but fun table is gone: must return error.
        ?assertEqual(error, beamtalk_class_metadata:lookup_class_method_fun('GateClass', f))
    after
        beamtalk_class_metadata:new(),
        ets:delete_all_objects(?TABLE),
        ets:delete_all_objects(?FUN_TABLE),
        ets:insert(?TABLE, SavedMeta),
        ets:insert(?FUN_TABLE, SavedFuns)
    end.

%% delete_class_method_funs/1 returns ok when the fun table does not exist.
%% Exercises the ets:info(?FUN_TABLE) =:= undefined branch (line ~430).
delete_class_method_funs_when_fun_table_absent_test() ->
    beamtalk_class_metadata:new(),
    SavedMeta = ets:tab2list(?TABLE),
    SavedFuns = ets:tab2list(?FUN_TABLE),
    ets:delete(?FUN_TABLE),
    try
        ?assertEqual(ok, beamtalk_class_metadata:delete_class_method_funs('AnyClass'))
    after
        beamtalk_class_metadata:new(),
        ets:delete_all_objects(?TABLE),
        ets:delete_all_objects(?FUN_TABLE),
        ets:insert(?TABLE, SavedMeta),
        ets:insert(?FUN_TABLE, SavedFuns)
    end.
