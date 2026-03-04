%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

%%% @doc EUnit tests for beamtalk_class_hierarchy_table (BT-1062).
%%%
%%% Tests the encapsulated ETS table API: new/0, insert/2, delete/1,
%%% lookup/1, foldl/2, and match_subclasses/1.

-module(beamtalk_class_hierarchy_table_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test setup helpers
%%% ============================================================================

%% Ensure the table exists and is clean for each test.
setup() ->
    beamtalk_class_hierarchy_table:new(),
    Saved = ets:tab2list(beamtalk_class_hierarchy),
    ets:delete_all_objects(beamtalk_class_hierarchy),
    Saved.

cleanup(Saved) ->
    ets:delete_all_objects(beamtalk_class_hierarchy),
    ets:insert(beamtalk_class_hierarchy, Saved).

with_clean_table(Fun) ->
    Saved = setup(),
    try
        Fun()
    after
        cleanup(Saved)
    end.

%%% ============================================================================
%%% new/0 tests
%%% ============================================================================

new_creates_table_test() ->
    %% new/0 must be idempotent — calling it multiple times is safe.
    ok = beamtalk_class_hierarchy_table:new(),
    ok = beamtalk_class_hierarchy_table:new(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_hierarchy)).

%%% ============================================================================
%%% insert/2 and lookup/1 tests
%%% ============================================================================

insert_and_lookup_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        ?assertEqual({ok, 'Actor'}, beamtalk_class_hierarchy_table:lookup('Counter'))
    end).

insert_with_none_superclass_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('ProtoObject', none),
        ?assertEqual({ok, none}, beamtalk_class_hierarchy_table:lookup('ProtoObject'))
    end).

lookup_missing_class_test() ->
    with_clean_table(fun() ->
        ?assertEqual(not_found, beamtalk_class_hierarchy_table:lookup('NonExistent'))
    end).

insert_overwrites_existing_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Object'),
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        ?assertEqual({ok, 'Actor'}, beamtalk_class_hierarchy_table:lookup('Counter'))
    end).

%%% ============================================================================
%%% delete/1 tests
%%% ============================================================================

delete_removes_entry_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        ok = beamtalk_class_hierarchy_table:delete('Counter'),
        ?assertEqual(not_found, beamtalk_class_hierarchy_table:lookup('Counter'))
    end).

delete_nonexistent_is_safe_test() ->
    with_clean_table(fun() ->
        ?assertEqual(ok, beamtalk_class_hierarchy_table:delete('DoesNotExist'))
    end).

%%% ============================================================================
%%% foldl/2 tests
%%% ============================================================================

foldl_collects_all_entries_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Object', none),
        ok = beamtalk_class_hierarchy_table:insert('Actor', 'Object'),
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        Result = beamtalk_class_hierarchy_table:foldl(
            fun({Class, Super}, Acc) -> Acc#{Class => Super} end,
            #{}
        ),
        ?assertEqual(
            #{
                'Object' => none,
                'Actor' => 'Object',
                'Counter' => 'Actor'
            },
            Result
        )
    end).

foldl_empty_table_returns_acc_test() ->
    with_clean_table(fun() ->
        Result = beamtalk_class_hierarchy_table:foldl(fun(_, Acc) -> Acc end, sentinel),
        ?assertEqual(sentinel, Result)
    end).

foldl_skips_none_superclass_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Object', none),
        ok = beamtalk_class_hierarchy_table:insert('Actor', 'Object'),
        %% Mirrors build_class_superclass_index which skips none entries.
        FoldFun = fun
            ({_Class, none}, Acc) ->
                Acc;
            ({Class, Superclass}, Acc) when is_atom(Class), is_atom(Superclass) ->
                Acc#{atom_to_binary(Class, utf8) => atom_to_binary(Superclass, utf8)};
            (_, Acc) ->
                Acc
        end,
        Result = beamtalk_class_hierarchy_table:foldl(FoldFun, #{}),
        ?assertEqual(#{<<"Actor">> => <<"Object">>}, Result)
    end).

%%% ============================================================================
%%% match_subclasses/1 tests
%%% ============================================================================

match_subclasses_finds_direct_children_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Object', none),
        ok = beamtalk_class_hierarchy_table:insert('Actor', 'Object'),
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        ok = beamtalk_class_hierarchy_table:insert('Timer', 'Actor'),
        Result = lists:sort(beamtalk_class_hierarchy_table:match_subclasses('Actor')),
        ?assertEqual(['Counter', 'Timer'], Result)
    end).

match_subclasses_no_children_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Counter', 'Actor'),
        ?assertEqual([], beamtalk_class_hierarchy_table:match_subclasses('Counter'))
    end).

match_subclasses_root_class_test() ->
    with_clean_table(fun() ->
        ok = beamtalk_class_hierarchy_table:insert('Object', none),
        ok = beamtalk_class_hierarchy_table:insert('Actor', 'Object'),
        ?assertEqual(['Actor'], beamtalk_class_hierarchy_table:match_subclasses('Object'))
    end).
