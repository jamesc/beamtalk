%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_extensions module.
%%%
%%% Tests extension registration, lookup, conflict tracking, and tooling APIs.

-module(beamtalk_extensions_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Setup: Initialize and clear tables before each test
setup() ->
    beamtalk_extensions:init(),
    %% Clear any existing data
    try
        ets:delete_all_objects(beamtalk_extensions)
    catch
        _:_ -> ok
    end,
    try
        ets:delete_all_objects(beamtalk_extension_conflicts)
    catch
        _:_ -> ok
    end,
    ok.

%% Cleanup: Clear tables after each test
cleanup(_) ->
    %% Delete all objects from both tables
    try
        ets:delete_all_objects(beamtalk_extensions)
    catch
        _:_ -> ok
    end,
    try
        ets:delete_all_objects(beamtalk_extension_conflicts)
    catch
        _:_ -> ok
    end,
    ok.

%%% ============================================================================
%%% Initialization tests
%%% ============================================================================

init_creates_tables_test() ->
    %% Clean up any existing tables
    catch ets:delete(beamtalk_extensions),
    catch ets:delete(beamtalk_extension_conflicts),

    beamtalk_extensions:init(),

    ?assertNotEqual(undefined, ets:info(beamtalk_extensions)),
    ?assertNotEqual(undefined, ets:info(beamtalk_extension_conflicts)).

init_is_idempotent_test() ->
    beamtalk_extensions:init(),
    %% Should not crash
    beamtalk_extensions:init(),

    ?assertNotEqual(undefined, ets:info(beamtalk_extensions)),
    ?assertNotEqual(undefined, ets:info(beamtalk_extension_conflicts)).

%%% ============================================================================
%%% Registration tests
%%% ============================================================================

register_new_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X * 2 end,

            ?assertEqual(ok, beamtalk_extensions:register('Integer', 'double', Fun, mylib)),

            %% Verify it's stored
            ?assertEqual({ok, Fun, mylib}, beamtalk_extensions:lookup('Integer', 'double'))
        end
    end}.

register_updates_same_owner_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X * 2 end,
            Fun2 = fun([], X) -> X * 3 end,

            beamtalk_extensions:register('Integer', 'multiply', Fun1, mylib),
            beamtalk_extensions:register('Integer', 'multiply', Fun2, mylib),

            %% Should use latest function from same owner
            ?assertEqual({ok, Fun2, mylib}, beamtalk_extensions:lookup('Integer', 'multiply'))
        end
    end}.

register_conflict_different_owner_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X * 2 end,
            Fun2 = fun([], X) -> X * 10 end,

            beamtalk_extensions:register('String', 'json', Fun1, mylib),
            beamtalk_extensions:register('String', 'json', Fun2, otherlib),

            %% Last writer wins
            ?assertEqual({ok, Fun2, otherlib}, beamtalk_extensions:lookup('String', 'json')),

            %% Conflict should be recorded
            Conflicts = beamtalk_extensions:conflicts(),
            ?assert(
                lists:any(
                    fun
                        ({'String', json, Owners}) ->
                            lists:member(mylib, Owners) andalso lists:member(otherlib, Owners);
                        (_) ->
                            false
                    end,
                    Conflicts
                )
            )
        end
    end}.

register_multiple_methods_on_same_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X end,
            Fun2 = fun([], X) -> X end,
            Fun3 = fun([], X) -> X end,

            beamtalk_extensions:register('String', 'json', Fun1, mylib),
            beamtalk_extensions:register('String', 'trim', Fun2, mylib),
            beamtalk_extensions:register('String', 'camelize', Fun3, mylib),

            %% All should be registered
            ?assertEqual({ok, Fun1, mylib}, beamtalk_extensions:lookup('String', 'json')),
            ?assertEqual({ok, Fun2, mylib}, beamtalk_extensions:lookup('String', 'trim')),
            ?assertEqual({ok, Fun3, mylib}, beamtalk_extensions:lookup('String', 'camelize'))
        end
    end}.

register_same_method_on_different_classes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,

            beamtalk_extensions:register('Integer', 'asString', Fun, stdlib),
            beamtalk_extensions:register('Float', 'asString', Fun, stdlib),
            beamtalk_extensions:register('Boolean', 'asString', Fun, stdlib),

            %% All should be registered independently
            ?assertEqual({ok, Fun, stdlib}, beamtalk_extensions:lookup('Integer', 'asString')),
            ?assertEqual({ok, Fun, stdlib}, beamtalk_extensions:lookup('Float', 'asString')),
            ?assertEqual({ok, Fun, stdlib}, beamtalk_extensions:lookup('Boolean', 'asString'))
        end
    end}.

%%% ============================================================================
%%% Lookup tests
%%% ============================================================================

lookup_existing_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X + 1 end,
            beamtalk_extensions:register('Integer', 'succ', Fun, stdlib),

            ?assertEqual({ok, Fun, stdlib}, beamtalk_extensions:lookup('Integer', 'succ'))
        end
    end}.

lookup_nonexistent_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assertEqual(not_found, beamtalk_extensions:lookup('Integer', 'unknownMethod'))
        end
    end}.

lookup_wrong_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'method', Fun, mylib),

            %% Method exists on Integer, but not on String
            ?assertEqual(not_found, beamtalk_extensions:lookup('String', 'method'))
        end
    end}.

%%% ============================================================================
%%% list/1 tests
%%% ============================================================================

list_empty_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assertEqual([], beamtalk_extensions:list('Integer'))
        end
    end}.

list_single_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('String', 'json', Fun, mylib),

            ?assertEqual([{json, mylib}], beamtalk_extensions:list('String'))
        end
    end}.

list_multiple_extensions_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('String', 'json', Fun, mylib),
            beamtalk_extensions:register('String', 'trim', Fun, stdlib),
            beamtalk_extensions:register('String', 'camelize', Fun, mylib),

            List = beamtalk_extensions:list('String'),
            ?assertEqual(3, length(List)),
            ?assert(lists:member({json, mylib}, List)),
            ?assert(lists:member({trim, stdlib}, List)),
            ?assert(lists:member({camelize, mylib}, List))
        end
    end}.

list_does_not_include_other_classes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('String', 'method1', Fun, mylib),
            beamtalk_extensions:register('Integer', 'method2', Fun, mylib),
            beamtalk_extensions:register('Float', 'method3', Fun, mylib),

            StringList = beamtalk_extensions:list('String'),
            ?assertEqual([{method1, mylib}], StringList)
        end
    end}.

%%% ============================================================================
%%% has/2 tests
%%% ============================================================================

has_existing_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),

            ?assert(beamtalk_extensions:has('Integer', 'double'))
        end
    end}.

has_nonexistent_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assertNot(beamtalk_extensions:has('Integer', 'unknownMethod'))
        end
    end}.

has_after_overwrite_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X end,
            Fun2 = fun([], X) -> X * 2 end,

            beamtalk_extensions:register('String', 'json', Fun1, lib1),
            beamtalk_extensions:register('String', 'json', Fun2, lib2),

            %% Should still exist after conflict
            ?assert(beamtalk_extensions:has('String', 'json'))
        end
    end}.

%%% ============================================================================
%%% conflicts/0 tests
%%% ============================================================================

conflicts_empty_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assertEqual([], beamtalk_extensions:conflicts())
        end
    end}.

conflicts_no_conflict_same_owner_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X end,
            Fun2 = fun([], X) -> X * 2 end,

            beamtalk_extensions:register('Integer', 'method', Fun1, mylib),
            beamtalk_extensions:register('Integer', 'method', Fun2, mylib),

            %% No conflict - same owner
            ?assertEqual([], beamtalk_extensions:conflicts())
        end
    end}.

conflicts_with_different_owners_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun1 = fun([], X) -> X end,
            Fun2 = fun([], X) -> X * 2 end,

            beamtalk_extensions:register('String', 'json', Fun1, mylib),
            beamtalk_extensions:register('String', 'json', Fun2, otherlib),

            Conflicts = beamtalk_extensions:conflicts(),
            ?assertEqual(1, length(Conflicts)),

            [{'String', json, Owners}] = Conflicts,
            ?assert(lists:member(mylib, Owners)),
            ?assert(lists:member(otherlib, Owners))
        end
    end}.

conflicts_multiple_methods_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,

            %% Conflict on String:json
            beamtalk_extensions:register('String', 'json', Fun, lib1),
            beamtalk_extensions:register('String', 'json', Fun, lib2),

            %% Conflict on Integer:parse
            beamtalk_extensions:register('Integer', 'parse', Fun, libA),
            beamtalk_extensions:register('Integer', 'parse', Fun, libB),

            Conflicts = beamtalk_extensions:conflicts(),
            ?assertEqual(2, length(Conflicts)),

            %% Check both conflicts are recorded
            ?assert(
                lists:any(
                    fun
                        ({'String', json, _}) -> true;
                        (_) -> false
                    end,
                    Conflicts
                )
            ),
            ?assert(
                lists:any(
                    fun
                        ({'Integer', parse, _}) -> true;
                        (_) -> false
                    end,
                    Conflicts
                )
            )
        end
    end}.

conflicts_three_way_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,

            beamtalk_extensions:register('String', 'json', Fun, lib1),
            beamtalk_extensions:register('String', 'json', Fun, lib2),
            beamtalk_extensions:register('String', 'json', Fun, lib3),

            Conflicts = beamtalk_extensions:conflicts(),
            ?assertEqual(1, length(Conflicts)),

            [{'String', json, Owners}] = Conflicts,
            ?assertEqual(3, length(Owners)),
            ?assert(lists:member(lib1, Owners)),
            ?assert(lists:member(lib2, Owners)),
            ?assert(lists:member(lib3, Owners))
        end
    end}.
