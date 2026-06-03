%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_extensions_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_extensions module.

Tests extension registration, lookup, conflict tracking, and tooling APIs.
""".
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
        ets:delete_all_objects(beamtalk_extension_sources)
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
        ets:delete_all_objects(beamtalk_extension_sources)
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
    (try
        ets:delete(beamtalk_extensions)
    catch
        _:_ -> ok
    end),
    (try
        ets:delete(beamtalk_extension_sources)
    catch
        _:_ -> ok
    end),
    (try
        ets:delete(beamtalk_extension_conflicts)
    catch
        _:_ -> ok
    end),

    beamtalk_extensions:init(),

    ?assertNotEqual(undefined, ets:info(beamtalk_extensions)),
    ?assertNotEqual(undefined, ets:info(beamtalk_extension_sources)),
    ?assertNotEqual(undefined, ets:info(beamtalk_extension_conflicts)).

init_is_idempotent_test() ->
    beamtalk_extensions:init(),
    %% Should not crash
    beamtalk_extensions:init(),

    ?assertNotEqual(undefined, ets:info(beamtalk_extensions)),
    ?assertNotEqual(undefined, ets:info(beamtalk_extension_sources)),
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

%%% ============================================================================
%%% BT-2196: Source tracking tests (register/5, getSource/2,
%%% listAllWithSource/0)
%%% ============================================================================

register_with_source_stores_source_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X * 2 end,
            Source = <<"double => self * 2">>,

            ?assertEqual(
                ok,
                beamtalk_extensions:register('Integer', 'double', Fun, mylib, Source)
            ),

            %% The dispatch entry should still be present (register/5 is a
            %% superset of register/4 — same lookup contract).
            ?assertEqual(
                {ok, Fun, mylib},
                beamtalk_extensions:lookup('Integer', 'double')
            ),

            %% Source is retrievable.
            ?assertEqual(
                {ok, Source},
                beamtalk_extensions:getSource('Integer', 'double')
            )
        end
    end}.

register_arity4_does_not_store_source_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,

            ?assertEqual(
                ok,
                beamtalk_extensions:register('Integer', 'noop', Fun, mylib)
            ),

            %% No source recorded for register/4 entries — keeps the source
            %% scan deliberately empty rather than surfacing entries with no
            %% body to grep over.
            ?assertEqual(
                not_found,
                beamtalk_extensions:getSource('Integer', 'noop')
            )
        end
    end}.

register_with_undefined_source_skips_source_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,

            %% Passing undefined is equivalent to register/4 — caller
            %% does not have source text to share.
            ?assertEqual(
                ok,
                beamtalk_extensions:register('Integer', 'noop2', Fun, mylib, undefined)
            ),

            ?assertEqual(
                not_found,
                beamtalk_extensions:getSource('Integer', 'noop2')
            )
        end
    end}.

get_source_missing_extension_returns_not_found_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assertEqual(
                not_found,
                beamtalk_extensions:getSource('NotARegisteredClass', 'noSuchSelector')
            )
        end
    end}.

list_all_with_source_returns_only_register5_entries_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            Src1 = <<"double => self * 2">>,
            Src2 = <<"trim => self">>,

            %% Two entries with source.
            beamtalk_extensions:register('Integer', 'double', Fun, mylib, Src1),
            beamtalk_extensions:register('String', 'trim', Fun, mylib, Src2),
            %% One entry without source (register/4).
            beamtalk_extensions:register('String', 'shout', Fun, mylib),

            Listed = beamtalk_extensions:listAllWithSource(),

            %% list_all_with_source/0 only surfaces entries with stored
            %% source. Order is ETS-dependent, so check membership.
            ?assertEqual(2, length(Listed)),
            ?assert(lists:member({'Integer', double, Src1}, Listed)),
            ?assert(lists:member({'String', trim, Src2}, Listed))
        end
    end}.

list_all_with_source_handles_uninitialised_table_test() ->
    %% Delete the sources table to simulate the early bootstrap window —
    %% must return [] rather than crashing. Wrap in try/after so a
    %% failing assertion still re-creates the table; otherwise the
    %% missing-table state cascades into every later test in the run.
    (try
        ets:delete(beamtalk_extension_sources)
    catch
        _:_ -> ok
    end),

    try
        ?assertEqual([], beamtalk_extensions:listAllWithSource())
    after
        beamtalk_extensions:init()
    end.

register_with_source_updates_source_on_reregister_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            Src1 = <<"original source">>,
            Src2 = <<"updated source">>,

            beamtalk_extensions:register('Integer', 'go', Fun, mylib, Src1),
            beamtalk_extensions:register('Integer', 'go', Fun, mylib, Src2),

            %% Latest source wins (mirrors last-writer-wins dispatch entry).
            ?assertEqual({ok, Src2}, beamtalk_extensions:getSource('Integer', 'go'))
        end
    end}.

register_arity4_clears_stale_source_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            Src = <<"original body for register/5">>,

            %% First register with source so the sources table has an entry.
            beamtalk_extensions:register('Integer', 'flip', Fun, mylib, Src),
            ?assertEqual({ok, Src}, beamtalk_extensions:getSource('Integer', 'flip')),

            %% Re-register via the legacy 4-arity path. The dispatch entry
            %% updates as usual, but the old source body must NOT linger —
            %% otherwise listAllWithSource/0 would still surface this
            %% extension and attribute the stale body to a registration
            %% that no longer carries source.
            beamtalk_extensions:register('Integer', 'flip', Fun, mylib),

            ?assertEqual(not_found, beamtalk_extensions:getSource('Integer', 'flip')),
            %% listAllWithSource/0 must not include the stale entry either.
            Listed = beamtalk_extensions:listAllWithSource(),
            ?assertNot(
                lists:any(fun({_C, S, _Src}) -> S =:= 'flip' end, Listed)
            )
        end
    end}.

register_with_undefined_clears_stale_source_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            Src = <<"original body">>,

            %% Seed with source via register/5.
            beamtalk_extensions:register('Integer', 'spin', Fun, mylib, Src),
            ?assertEqual({ok, Src}, beamtalk_extensions:getSource('Integer', 'spin')),

            %% Explicit undefined source must also wipe the stale row.
            beamtalk_extensions:register('Integer', 'spin', Fun, mylib, undefined),
            ?assertEqual(not_found, beamtalk_extensions:getSource('Integer', 'spin'))
        end
    end}.

get_source_handles_uninitialised_table_test() ->
    %% Mirrors list_all_with_source_handles_uninitialised_table_test —
    %% getSource/2 must return not_found (per its spec) rather than
    %% letting the ETS badarg escape when the sources table doesn't
    %% exist yet during early bootstrap. Same try/after recovery as
    %% the sibling test so a failed assertion does not leak the
    %% missing-table state into the rest of the suite.
    (try
        ets:delete(beamtalk_extension_sources)
    catch
        _:_ -> ok
    end),

    try
        ?assertEqual(not_found, beamtalk_extensions:getSource('Anything', 'whatever'))
    after
        beamtalk_extensions:init()
    end.

%%% ============================================================================
%%% BT-2202: extenders_of/1 tests
%%% ============================================================================

extenders_of_empty_table_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            %% No extensions registered — must return [] not an error.
            ?assertEqual([], beamtalk_extensions:extenders_of('Integer'))
        end
    end}.

extenders_of_single_extender_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X * 2 end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),

            ?assertEqual([mylib], beamtalk_extensions:extenders_of('Integer'))
        end
    end}.

extenders_of_multiple_extenders_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),
            beamtalk_extensions:register('Integer', 'triple', Fun, stdlib),
            beamtalk_extensions:register('Integer', 'negate', Fun, otherlib),

            Result = beamtalk_extensions:extenders_of('Integer'),
            %% Three distinct owners.
            ?assertEqual(3, length(Result)),
            ?assert(lists:member(mylib, Result)),
            ?assert(lists:member(stdlib, Result)),
            ?assert(lists:member(otherlib, Result))
        end
    end}.

extenders_of_deduplicates_same_owner_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            %% Same owner registers multiple methods on the same class.
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),
            beamtalk_extensions:register('Integer', 'triple', Fun, mylib),
            beamtalk_extensions:register('Integer', 'negate', Fun, mylib),

            %% Owner should appear only once despite multiple methods.
            ?assertEqual([mylib], beamtalk_extensions:extenders_of('Integer'))
        end
    end}.

extenders_of_does_not_include_other_class_owners_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('String', 'json', Fun, lib1),
            beamtalk_extensions:register('Integer', 'double', Fun, lib2),
            beamtalk_extensions:register('Float', 'ceil', Fun, lib3),

            %% Query for String should only return lib1, not lib2 or lib3.
            ?assertEqual([lib1], beamtalk_extensions:extenders_of('String'))
        end
    end}.

extenders_of_no_match_returns_empty_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),

            %% Asking about a class with no extensions returns [].
            ?assertEqual([], beamtalk_extensions:extenders_of('Float'))
        end
    end}.

%%% ============================================================================
%%% BT-2202: extensions_by/1 tests
%%% ============================================================================

extensions_by_empty_table_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            %% No extensions registered — must return [] not an error.
            ?assertEqual([], beamtalk_extensions:extensions_by(mylib))
        end
    end}.

extensions_by_single_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X * 2 end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),

            ?assertEqual([{'Integer', double}], beamtalk_extensions:extensions_by(mylib))
        end
    end}.

extensions_by_multiple_targets_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),
            beamtalk_extensions:register('String', 'json', Fun, mylib),
            beamtalk_extensions:register('Float', 'ceil', Fun, mylib),

            Result = beamtalk_extensions:extensions_by(mylib),
            %% Three extensions contributed by mylib across different classes.
            ?assertEqual(3, length(Result)),
            ?assert(lists:member({'Integer', double}, Result)),
            ?assert(lists:member({'String', json}, Result)),
            ?assert(lists:member({'Float', ceil}, Result))
        end
    end}.

extensions_by_excludes_other_owners_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),
            beamtalk_extensions:register('String', 'json', Fun, otherlib),
            beamtalk_extensions:register('Float', 'ceil', Fun, mylib),

            %% Only mylib's extensions, not otherlib's.
            Result = beamtalk_extensions:extensions_by(mylib),
            ?assertEqual(2, length(Result)),
            ?assert(lists:member({'Integer', double}, Result)),
            ?assert(lists:member({'Float', ceil}, Result)),
            ?assertNot(lists:member({'String', json}, Result))
        end
    end}.

extensions_by_no_match_returns_empty_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            beamtalk_extensions:register('Integer', 'double', Fun, mylib),

            %% Unknown owner has no extensions.
            ?assertEqual([], beamtalk_extensions:extensions_by(unknown_owner))
        end
    end}.

%%% ============================================================================
%%% BT-2202: Symmetric round-trip tests
%%% ============================================================================

extenders_of_extensions_by_round_trip_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Fun = fun([], X) -> X end,
            %% lib1 extends Integer and String.
            beamtalk_extensions:register('Integer', 'double', Fun, lib1),
            beamtalk_extensions:register('String', 'json', Fun, lib1),
            %% lib2 extends Integer only.
            beamtalk_extensions:register('Integer', 'triple', Fun, lib2),

            %% extenders_of('Integer') should include both lib1 and lib2.
            IntExtenders = beamtalk_extensions:extenders_of('Integer'),
            ?assertEqual(2, length(IntExtenders)),
            ?assert(lists:member(lib1, IntExtenders)),
            ?assert(lists:member(lib2, IntExtenders)),

            %% extenders_of('String') should include only lib1.
            StrExtenders = beamtalk_extensions:extenders_of('String'),
            ?assertEqual([lib1], StrExtenders),

            %% extensions_by(lib1) should include both {Integer,double} and {String,json}.
            Lib1Exts = beamtalk_extensions:extensions_by(lib1),
            ?assertEqual(2, length(Lib1Exts)),
            ?assert(lists:member({'Integer', double}, Lib1Exts)),
            ?assert(lists:member({'String', json}, Lib1Exts)),

            %% extensions_by(lib2) should include only {Integer,triple}.
            Lib2Exts = beamtalk_extensions:extensions_by(lib2),
            ?assertEqual([{'Integer', triple}], Lib2Exts),

            %% Symmetry: every owner returned by extenders_of/1 for a class
            %% must include that class in extensions_by/1 for that owner.
            lists:foreach(
                fun(Owner) ->
                    OwnerExts = beamtalk_extensions:extensions_by(Owner),
                    TargetClasses = [TC || {TC, _Sel} <- OwnerExts],
                    ?assert(lists:member('Integer', TargetClasses))
                end,
                IntExtenders
            )
        end
    end}.

%%% ============================================================================
%%% ADR 0087 Phase 4 (BT-2301): xref index hooks for extension lifecycle
%%% ============================================================================

%% Setup that also stands up (and clears) a beamtalk_xref gen_server so the
%% extension register/unregister hooks have somewhere to write.
xref_setup() ->
    setup(),
    case whereis(beamtalk_xref) of
        undefined -> {ok, _} = beamtalk_xref:start_link();
        _ -> ok
    end,
    clear_xref_tables(),
    ok.

xref_cleanup(_) ->
    clear_xref_tables(),
    cleanup(undefined),
    ok.

clear_xref_tables() ->
    try
        sys:replace_state(beamtalk_xref, fun(S) ->
            ets:delete_all_objects(beamtalk_xref_methods),
            ets:delete_all_objects(beamtalk_xref_senders),
            ets:delete_all_objects(beamtalk_xref_references),
            ets:delete_all_objects(xref_class_gen),
            S
        end)
    catch
        _:_ -> ok
    end,
    ok.

%% True when the compiler AST FFI walker that backs runtime xref send-parsing
%% is loaded and reachable. A bare EUnit node may lack the compiler port, in
%% which case the index degrades to empty sends; send-row assertions gate on
%% this so they don't fail spuriously.
compiler_walker_available() ->
    erlang:function_exported(beamtalk_compiler, find_all_sends_in_source, 1) andalso
        case catch beamtalk_compiler:find_all_sends_in_source(<<"x => self foo">>) of
            {ok, [_ | _]} -> true;
            _ -> false
        end.

register5_indexes_sends_in_xref_test_() ->
    {setup, fun xref_setup/0, fun xref_cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% A sourced extension (register/5) is parsed and indexed: the
                %% method becomes an implementor, and its sends are visible.
                Fun = fun(_Args, _Self) -> ok end,
                Source = <<"shout => self asString asUppercase">>,
                ok = beamtalk_extensions:register('String', 'shout', Fun, mylib, Source),

                ?assertEqual(
                    [{'String', false}], beamtalk_xref:implementors_of('shout')
                ),
                Info = beamtalk_xref:method_info('String', false, 'shout'),
                ?assertEqual(indexed, maps:get(source_status, Info)),
                ?assertEqual(extension, maps:get(provenance, Info)),

                %% `asUppercase` is sent by the extension body — visible only when
                %% the compiler AST walker is available (a bare EUnit node without
                %% the compiler port degrades to empty sends, but the indexed
                %% method row above is always recorded).
                case compiler_walker_available() of
                    false ->
                        ok;
                    true ->
                        UpperSites = beamtalk_xref:senders_of('asUppercase'),
                        ?assert(
                            lists:any(
                                fun(S) ->
                                    maps:get(owner, S) =:= 'String' andalso
                                        maps:get(method, S) =:= 'shout'
                                end,
                                UpperSites
                            )
                        )
                end
            end)
        ]
    end}.

register4_marks_unindexed_runtime_fun_test_() ->
    {setup, fun xref_setup/0, fun xref_cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% A sourceless extension (register/4) is recorded as an
                %% unindexed_runtime_fun marker: present as an implementor, but
                %% with no scannable sends.
                Fun = fun(_Args, _Self) -> ok end,
                ok = beamtalk_extensions:register('Integer', 'doubled', Fun, mylib),

                ?assertEqual(
                    [{'Integer', false}], beamtalk_xref:implementors_of('doubled')
                ),
                Info = beamtalk_xref:method_info('Integer', false, 'doubled'),
                ?assertEqual(unindexed_runtime_fun, maps:get(source_status, Info)),
                ?assertEqual(extension, maps:get(provenance, Info))
            end)
        ]
    end}.

unregister_purges_xref_rows_test_() ->
    {setup, fun xref_setup/0, fun xref_cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Register two extensions on the same class, then unregister one
                %% — only its rows are purged; the sibling stays.
                Fun = fun(_Args, _Self) -> ok end,
                ok = beamtalk_extensions:register(
                    'String', 'shout', Fun, mylib, <<"shout => self asUppercase">>
                ),
                ok = beamtalk_extensions:register(
                    'String', 'whisper', Fun, mylib, <<"whisper => self asLowercase">>
                ),
                ?assertEqual([{'String', false}], beamtalk_xref:implementors_of('shout')),
                ?assertEqual([{'String', false}], beamtalk_xref:implementors_of('whisper')),

                ok = beamtalk_extensions:unregister('String', 'shout'),

                %% `shout` gone from dispatch, sources, and xref.
                ?assertEqual(not_found, beamtalk_extensions:lookup('String', 'shout')),
                ?assertEqual(not_found, beamtalk_extensions:getSource('String', 'shout')),
                ?assertEqual([], beamtalk_xref:implementors_of('shout')),
                ?assertEqual([], beamtalk_xref:senders_of('asUppercase')),

                %% Sibling `whisper` untouched — its method row survives the
                %% narrow purge regardless of compiler availability.
                ?assertEqual([{'String', false}], beamtalk_xref:implementors_of('whisper')),
                case compiler_walker_available() of
                    false ->
                        ok;
                    true ->
                        ?assertEqual(1, length(beamtalk_xref:senders_of('asLowercase')))
                end
            end)
        ]
    end}.

unregister_unknown_is_noop_test_() ->
    {setup, fun xref_setup/0, fun xref_cleanup/1, fun(_) ->
        [
            ?_test(begin
                %% Unregistering an extension that was never registered is a
                %% harmless no-op.
                ?assertEqual(ok, beamtalk_extensions:unregister('String', 'nope'))
            end)
        ]
    end}.
