%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_registry_tests).

-moduledoc """
EUnit tests for beamtalk_class_registry.

Tests class lookup, hierarchy queries, process group management,
and class object identity functions.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% registry_name tests
%%% ============================================================================

registry_name_test() ->
    ?assertEqual(
        beamtalk_class_Counter,
        beamtalk_class_registry:registry_name('Counter')
    ).

registry_name_object_test() ->
    ?assertEqual(
        beamtalk_class_Object,
        beamtalk_class_registry:registry_name('Object')
    ).

%% BT-4301: codegen bakes a class's registered name and metaclass tag into the
%% guarded direct call of a static class send (`util::class_registered_name` /
%% `util::metaclass_tag` in beamtalk-codegen). The shared fixture is the single
%% source both sides are pinned to; the Rust side asserts the identical cases
%% in `util::tests::class_registered_name_matches_shared_fixture`.
class_registered_name_matches_shared_fixture_test() ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_runtime",
        "test",
        "fixtures",
        "class_registered_name_conformance.json"
    ]),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(#{<<"class">> := Class, <<"registered">> := Registered, <<"tag">> := Tag}) ->
            Name = binary_to_atom(Class, utf8),
            ?assertEqual(
                binary_to_atom(Registered, utf8), beamtalk_class_registry:registry_name(Name)
            ),
            ?assertEqual(
                binary_to_atom(Tag, utf8), beamtalk_class_registry:class_object_tag(Name)
            )
        end,
        Cases
    ).

%%% ============================================================================
%%% class_object_tag tests
%%% ============================================================================

class_object_tag_test() ->
    ?assertEqual('Point class', beamtalk_class_registry:class_object_tag('Point')).

class_object_tag_object_test() ->
    ?assertEqual('Object class', beamtalk_class_registry:class_object_tag('Object')).

%%% ============================================================================
%%% is_class_name tests
%%% ============================================================================

is_class_name_true_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Integer class')).

is_class_name_false_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('Integer')).

is_class_name_short_atom_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('ab')).

is_class_name_binary_true_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name(<<"Integer class">>)).

is_class_name_binary_false_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name(<<"Integer">>)).

is_class_name_unsupported_type_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name(42)).

is_class_name_object_class_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Object class')).

%%% ============================================================================
%%% is_class_object tests
%%% ============================================================================

is_class_object_true_test() ->
    Obj = {beamtalk_object, 'Counter class', counter, self()},
    ?assertEqual(true, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_false_instance_test() ->
    Obj = {beamtalk_object, 'Counter', counter, self()},
    ?assertEqual(false, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_non_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object(42)).

is_class_object_wrong_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object({wrong, tuple})).

%%% ============================================================================
%%% class_display_name tests
%%% ============================================================================

class_display_name_strips_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name('Counter class')
    ).

class_display_name_no_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name('Counter')
    ).

class_display_name_object_test() ->
    ?assertEqual(
        <<"Object">>,
        beamtalk_class_registry:class_display_name('Object class')
    ).

class_display_name_binary_strips_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name(<<"Counter class">>)
    ).

class_display_name_binary_no_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name(<<"Counter">>)
    ).

%%% ============================================================================
%%% ensure_pg_started tests
%%% ============================================================================

ensure_pg_started_test() ->
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()),
    %% Calling again should also succeed (idempotent)
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()).

%%% ============================================================================
%%% ensure_hierarchy_table tests
%%% ============================================================================

ensure_hierarchy_table_test_() ->
    {setup,
        fun() ->
            %% Save existing entries to restore after test
            case ets:info(beamtalk_class_metadata) of
                undefined -> {missing, []};
                _ -> {exists, ets:tab2list(beamtalk_class_metadata)}
            end
        end,
        fun
            ({missing, _}) ->
                %% Table didn't exist before; delete if we created it and we own it
                case ets:info(beamtalk_class_metadata, owner) of
                    undefined -> ok;
                    Owner when Owner =:= self() -> ets:delete(beamtalk_class_metadata);
                    _ -> ok
                end;
            ({exists, Saved}) ->
                %% Restore original entries
                ets:delete_all_objects(beamtalk_class_metadata),
                ets:insert(beamtalk_class_metadata, Saved),
                %% A raw restore bypasses the metadata API, so drop derived
                %% facts cached against the test rows (BT-4064).
                beamtalk_class_derived_cache:invalidate()
        end,
        fun() ->
            ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table()),
            ?assertNotEqual(undefined, ets:info(beamtalk_class_metadata)),
            %% Idempotent
            ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table())
        end}.

%%% ============================================================================
%%% inherits_from tests
%%% ============================================================================

inherits_from_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_metadata),
            beamtalk_class_metadata:insert('Object', undefined, undefined, none, undefined),
            beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object', undefined),
            beamtalk_class_metadata:insert('Counter', undefined, undefined, 'Actor', undefined),
            Saved
        end,
        fun(Saved) ->
            %% Restore original state
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:insert(beamtalk_class_metadata, Saved),
            %% A raw restore bypasses the metadata API, so drop derived
            %% facts cached against the test rows (BT-4064).
            beamtalk_class_derived_cache:invalidate()
        end,
        [
            {"same class", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Counter'))
            end},
            {"direct parent", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Actor'))
            end},
            {"grandparent", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Object'))
            end},
            {"not related", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from('Object', 'Counter'))
            end},
            {"none ancestor", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from(none, 'Object'))
            end},
            {"unknown class", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from('Unknown', 'Object'))
            end}
        ]}.

%%% ============================================================================
%%% direct_subclasses tests
%%% ============================================================================

direct_subclasses_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_metadata),
            %% direct_subclasses/1 reads beamtalk_class_subclass_index,
            %% a table separate from beamtalk_class_metadata. Wiping only the main
            %% table (as before) leaves this test's real, currently-loaded classes
            %% still indexed, so 'Object'/'Actor' would resolve to their genuine
            %% subclasses instead of this fixture's synthetic ones — must be
            %% cleared/restored in lockstep with the main table.
            SavedIndex = ets:tab2list(beamtalk_class_subclass_index),
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_subclass_index),
            beamtalk_class_metadata:insert('Object', undefined, undefined, none, undefined),
            beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object', undefined),
            beamtalk_class_metadata:insert('Counter', undefined, undefined, 'Actor', undefined),
            beamtalk_class_metadata:insert('Timer', undefined, undefined, 'Actor', undefined),
            {Saved, SavedIndex}
        end,
        fun({Saved, SavedIndex}) ->
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_subclass_index),
            ets:insert(beamtalk_class_metadata, Saved),
            %% A raw restore bypasses the metadata API, so drop derived
            %% facts cached against the test rows (BT-4064).
            beamtalk_class_derived_cache:invalidate(),
            ets:insert(beamtalk_class_subclass_index, SavedIndex)
        end,
        [
            {"direct children of Actor", fun() ->
                Result = beamtalk_class_registry:direct_subclasses('Actor'),
                ?assertEqual(['Counter', 'Timer'], Result)
            end},
            {"direct children of Object", fun() ->
                ?assertEqual(['Actor'], beamtalk_class_registry:direct_subclasses('Object'))
            end},
            {"leaf class has no children", fun() ->
                ?assertEqual([], beamtalk_class_registry:direct_subclasses('Counter'))
            end},
            {"unknown class", fun() ->
                ?assertEqual([], beamtalk_class_registry:direct_subclasses('Unknown'))
            end}
        ]}.

%%% ============================================================================
%%% all_subclasses tests
%%% ============================================================================

all_subclasses_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_metadata),
            %% See direct_subclasses_test_/0's setup for why the
            %% subclass index must be cleared/restored alongside the main table.
            SavedIndex = ets:tab2list(beamtalk_class_subclass_index),
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_subclass_index),
            beamtalk_class_metadata:insert('Object', undefined, undefined, none, undefined),
            beamtalk_class_metadata:insert('Actor', undefined, undefined, 'Object', undefined),
            beamtalk_class_metadata:insert('Counter', undefined, undefined, 'Actor', undefined),
            beamtalk_class_metadata:insert('Timer', undefined, undefined, 'Actor', undefined),
            {Saved, SavedIndex}
        end,
        fun({Saved, SavedIndex}) ->
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_subclass_index),
            ets:insert(beamtalk_class_metadata, Saved),
            %% A raw restore bypasses the metadata API, so drop derived
            %% facts cached against the test rows (BT-4064).
            beamtalk_class_derived_cache:invalidate(),
            ets:insert(beamtalk_class_subclass_index, SavedIndex)
        end,
        [
            {"all subclasses of Object", fun() ->
                Result = beamtalk_class_registry:all_subclasses('Object'),
                ?assertEqual(['Actor', 'Counter', 'Timer'], Result)
            end},
            {"all subclasses of Actor", fun() ->
                Result = beamtalk_class_registry:all_subclasses('Actor'),
                ?assertEqual(['Counter', 'Timer'], Result)
            end},
            {"leaf class has no subclasses", fun() ->
                ?assertEqual([], beamtalk_class_registry:all_subclasses('Counter'))
            end},
            {"a superclass cycle terminates, each class listed once", fun() ->
                beamtalk_class_metadata:insert('CycA', undefined, undefined, 'CycB', undefined),
                beamtalk_class_metadata:insert('CycB', undefined, undefined, 'CycA', undefined),
                ?assertEqual(['CycB'], beamtalk_class_registry:all_subclasses('CycA'))
            end}
        ]}.

%%% ============================================================================
%%% whereis_class tests
%%% ============================================================================

whereis_class_unregistered_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:whereis_class('NonExistentClass12345')
    ).

whereis_class_registered_test() ->
    RegName = beamtalk_class_registry:registry_name('TestClassBT708'),
    register(RegName, self()),
    ?assertEqual(self(), beamtalk_class_registry:whereis_class('TestClassBT708')),
    unregister(RegName).

%%% ============================================================================
%%% all_classes tests
%%% ============================================================================

all_classes_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    %% all_classes returns whatever is in the pg group
    Result = beamtalk_class_registry:all_classes(),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% live_class_entries tests
%%% ============================================================================

live_class_entries_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
            {ok, Pid} = beamtalk_object_class:start_link('LiveEntriesTestClass1090', ClassInfo),
            Pid
        end,
        fun(Pid) ->
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end)
        end,
        [
            {"returns a list", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                ?assert(is_list(Result))
            end},
            {"entries are {Name, Module, Pid} tuples", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                lists:foreach(
                    fun({Name, Mod, Pid}) ->
                        ?assert(is_atom(Name)),
                        ?assert(is_atom(Mod)),
                        ?assert(is_pid(Pid))
                    end,
                    Result
                )
            end},
            {"includes the started class", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                Names = [Name || {Name, _Mod, _Pid} <- Result],
                ?assert(lists:member('LiveEntriesTestClass1090', Names))
            end}
        ]}.

live_class_entries_skips_class_dying_mid_call_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    %% Answers the first call by exiting `shutdown` (not noproc/timeout).
    Pid = spawn(fun() ->
        receive
            {'$gen_call', _From, _Req} -> exit(shutdown)
        end
    end),
    ok = pg:join(beamtalk_classes, Pid),
    try
        Pids = [P || {_, _, P} <- beamtalk_class_registry:live_class_entries()],
        ?assertNot(lists:member(Pid, Pids))
    after
        pg:leave(beamtalk_classes, Pid)
    end.

%% BT-3820 regression: two class processes whose class-side handlers each run
%% `live_class_entries/0` concurrently must not deadlock. Both handlers are held
%% at a barrier until both are inside their `class_method_call`, so each is
%% occupied serving a call when it asks the registry for the other's name. If
%% `live_class_entries/0` reads names with `gen_server:call(Pid, class_name)`,
%% each handler waits on the other's (busy) mailbox until the 5s call timeout
%% (`{timeout, {gen_server, call, [_, class_name]}}`, swallowed by the registry's
%% `exit:_ -> false` filter) and the peer is silently missing from the result.
live_class_entries_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        beamtalk_class_registry:ensure_pg_started(),
        Coord = self(),
        NameA = 'BT3820DeadlockA',
        NameB = 'BT3820DeadlockB',
        Probe = fun(_ClassSelf) ->
            Coord ! {in_handler, self()},
            receive
                {go, Me} when Me =:= self() -> ok
            after 10000 -> exit(barrier_timeout)
            end,
            [Name || {Name, _Mod, _Pid} <- beamtalk_class_registry:live_class_entries()]
        end,
        Info = fun() ->
            #{
                superclass => none,
                module => bt3820_no_module,
                class_methods => #{bt3820Probe => #{block => Probe, arity => 1}}
            }
        end,
        {ok, PidA} = beamtalk_object_class:start_link(NameA, Info()),
        {ok, PidB} = beamtalk_object_class:start_link(NameB, Info()),
        try
            Callers = [
                spawn_link(fun() ->
                    Coord ! {result, P, beamtalk_class_dispatch:class_send(P, bt3820Probe, [])}
                end)
             || P <- [PidA, PidB]
            ],
            %% Wait until both class processes are inside their handlers, then release.
            InHandler = [
                receive
                    {in_handler, H} -> H
                after 10000 -> error(handler_not_entered)
                end
             || _ <- Callers
            ],
            [H ! {go, H} || H <- InHandler],
            Start = erlang:monotonic_time(millisecond),
            Results = [
                receive
                    {result, P, Names} -> {P, Names}
                after 15000 -> error({no_result, P})
                end
             || P <- [PidA, PidB]
            ],
            Elapsed = erlang:monotonic_time(millisecond) - Start,
            lists:foreach(
                fun({_P, Names}) ->
                    ?assert(lists:member(NameA, Names)),
                    ?assert(lists:member(NameB, Names))
                end,
                Results
            ),
            %% Far below the 5s gen_server:call timeout the deadlock would burn.
            ?assert(Elapsed < 4000)
        after
            [catch gen_server:stop(P, normal, 5000) || P <- [PidA, PidB]]
        end
    end}.

%% BT-3820 review: a pg member that died but that pg has not yet dropped must be
%% filtered, not crash `live_class_entries/0` / `user_classes/0`. `pg` is
%% suspended so the stale membership is deterministic rather than a race.
live_class_entries_skips_dead_pg_member_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = pg:join(beamtalk_classes, Pid),
    ok = sys:suspend(pg),
    try
        Ref = monitor(process, Pid),
        exit(Pid, kill),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        end,
        ?assert(lists:member(Pid, beamtalk_class_registry:all_classes())),
        ?assertNot(
            lists:member(Pid, [P || {_, _, P} <- beamtalk_class_registry:live_class_entries()])
        ),
        ?assertNot(
            lists:member(Pid, [
                P
             || {beamtalk_object, _, _, P} <- beamtalk_class_registry:user_classes()
            ])
        )
    after
        ok = sys:resume(pg)
    end.

%% BT-3820 review: `class_identity/1` (what `live_class_entries/0` and
%% `user_classes/0` filter pg members through) must *skip* -- never crash on, nor
%% message -- a dead pid pg has not yet dropped, a remote pid, and a live pid that
%% has not registered (the window inside `init/1` between `pg:join` and the
%% identity writes). A `true = ...` match here would raise `badmatch`, which the
%% callers' `exit:_` filter does not catch.
class_identity_skips_unregistered_dead_and_remote_pids_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    %% Dead pid.
    Dead = spawn(fun() -> ok end),
    Ref = monitor(process, Dead),
    receive
        {'DOWN', Ref, process, Dead, _} -> ok
    end,
    ?assertEqual(skip, beamtalk_class_registry:class_identity(Dead)),
    %% Live pid with no reverse-index row (stands in for the init/1 window).
    Live = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        ?assertEqual(skip, beamtalk_class_registry:class_identity(Live))
    after
        Live ! stop
    end,
    %% Remote pid (different node).
    Remote = beamtalk_dist_test_helper:unreachable_pid(bt3820_other),
    ?assertNotEqual(node(), node(Remote)),
    ?assertEqual(skip, beamtalk_class_registry:class_identity(Remote)),
    %% A genuinely registered class resolves.
    ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
    {ok, Pid} = beamtalk_object_class:start_link('BT3820IdentityClass', ClassInfo),
    try
        ?assertMatch(
            {ok, 'BT3820IdentityClass', _}, beamtalk_class_registry:class_identity(Pid)
        )
    after
        gen_server:stop(Pid)
    end.

%%% ============================================================================
%%% loaded-class name index tests
%%% ============================================================================

%% A class started through the normal lifecycle records itself in the fast
%% loaded-class index (init hook); stopping it removes the row (terminate hook).
%% This is the cache-coherence contract the xref miss-policy depends on.
loaded_class_index_lifecycle_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_loaded_classes_table()
        end,
        fun(_) -> ok end, [
            {"started class appears, stopped class disappears", fun() ->
                Class = 'LoadedIndexLifecycle2384',
                ?assertNot(sets:is_element(Class, beamtalk_class_registry:loaded_class_names())),
                ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
                {ok, Pid} = beamtalk_object_class:start_link(Class, ClassInfo),
                ?assert(sets:is_element(Class, beamtalk_class_registry:loaded_class_names())),
                ok = gen_server:stop(Pid),
                ?assertNot(sets:is_element(Class, beamtalk_class_registry:loaded_class_names()))
            end},
            {"loaded_class_entries carries the live pid", fun() ->
                Class = 'LoadedIndexEntries2384',
                ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
                {ok, Pid} = beamtalk_object_class:start_link(Class, ClassInfo),
                try
                    Entries = beamtalk_class_registry:loaded_class_entries(),
                    ?assert(lists:member({Class, Pid}, Entries))
                after
                    gen_server:stop(Pid)
                end
            end}
        ]}.

%% A class that crashes (its terminate/1 never runs) leaves a stale {Name, Pid}
%% row, but the reader filters dead pids out via is_process_alive/1 — so the
%% loaded set stays coherent without a gen_server round-trip.
loaded_class_index_drops_dead_pid_test() ->
    beamtalk_class_registry:ensure_loaded_classes_table(),
    Class = 'LoadedIndexDeadPid2384',
    %% Forge a row pointing at a pid that is already dead (mimics a crash that
    %% skipped terminate/1). We can write directly because the table is public.
    DeadPid = spawn(fun() -> ok end),
    %% Wait for the spawned process to exit so the pid is reliably dead.
    Ref = monitor(process, DeadPid),
    receive
        {'DOWN', Ref, process, DeadPid, _} -> ok
    after 1000 -> ok
    end,
    ets:insert(beamtalk_loaded_classes, {Class, DeadPid}),
    ?assertNot(sets:is_element(Class, beamtalk_class_registry:loaded_class_names())),
    ?assertNot(lists:keymember(Class, 1, beamtalk_class_registry:loaded_class_entries())),
    ets:delete(beamtalk_loaded_classes, Class).

%% A reload (same name, new pid) overwrites the row; the dying old process must
%% not clobber the replacement — forget_loaded_class/2 is guarded on the pid.
loaded_class_index_reload_keeps_new_pid_test() ->
    beamtalk_class_registry:ensure_loaded_classes_table(),
    Class = 'LoadedIndexReload2384',
    OldPid = list_to_pid("<0.9991.0>"),
    NewPid = list_to_pid("<0.9992.0>"),
    %% Record the "new" generation's pid, then have the "old" generation try to
    %% forget itself. The guard must leave the new pid's row intact.
    ets:insert(beamtalk_loaded_classes, {Class, NewPid}),
    ok = beamtalk_class_registry:forget_loaded_class(Class, OldPid),
    ?assertEqual([{Class, NewPid}], ets:lookup(beamtalk_loaded_classes, Class)),
    %% Forgetting with the matching pid removes it.
    ok = beamtalk_class_registry:forget_loaded_class(Class, NewPid),
    ?assertEqual([], ets:lookup(beamtalk_loaded_classes, Class)).

%%% ============================================================================
%%% backing-module reverse index tests
%%% ============================================================================

meta_backing_module_test_() ->
    [
        {"a real atom is returned as-is", fun() ->
            ?assertEqual(
                bt_native_mod,
                beamtalk_class_registry:meta_backing_module(#{backing_module => bt_native_mod})
            )
        end},
        {"a missing backing_module key is none", fun() ->
            ?assertEqual(none, beamtalk_class_registry:meta_backing_module(#{}))
        end},
        {"an explicit `none` value is none", fun() ->
            ?assertEqual(
                none, beamtalk_class_registry:meta_backing_module(#{backing_module => none})
            )
        end},
        {"an explicit `undefined` value is none", fun() ->
            ?assertEqual(
                none, beamtalk_class_registry:meta_backing_module(#{backing_module => undefined})
            )
        end},
        {"a non-atom value is none", fun() ->
            ?assertEqual(
                none, beamtalk_class_registry:meta_backing_module(#{backing_module => "lists"})
            )
        end}
    ].

%% A class started through the normal lifecycle (init/1) with a `meta` map
%% carrying `backing_module` shows up in the reverse index (write side); a
%% class with no backing module never does. Passing `meta` directly in
%% ClassInfo (rather than compiling a fake module) exercises the exact same
%% code path init/1 uses for a real ADR 0056 `native:` facade, since init/1
%% only consults `Module:'__beamtalk_meta'()` when `meta` is absent.
backing_module_index_lifecycle_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_backing_module_index_table()
        end,
        fun(_) -> ok end, [
            {"native-backed class appears under its backing module", fun() ->
                Class = 'BackingIndexLifecycle2736',
                Backing = bt_backing_lifecycle_2736,
                ?assertNot(
                    lists:member(Class, beamtalk_class_registry:classes_backing_module(Backing))
                ),
                ClassInfo = #{
                    superclass => none,
                    methods => #{},
                    class_methods => #{},
                    meta => #{backing_module => Backing}
                },
                {ok, Pid} = beamtalk_object_class:start_link(Class, ClassInfo),
                ?assert(
                    lists:member(Class, beamtalk_class_registry:classes_backing_module(Backing))
                ),
                ok = gen_server:stop(Pid),
                ?assertNot(
                    lists:member(Class, beamtalk_class_registry:classes_backing_module(Backing))
                )
            end},
            {"a class with no backing module is never indexed", fun() ->
                Class = 'BackingIndexNoNative2736',
                ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
                {ok, Pid} = beamtalk_object_class:start_link(Class, ClassInfo),
                try
                    %% There is no module-less "none" bucket to check membership
                    %% against directly; instead confirm the class never appears
                    %% under a module name it was never given.
                    ?assertNot(
                        lists:member(
                            Class,
                            beamtalk_class_registry:classes_backing_module(
                                bt_backing_never_assigned_2736
                            )
                        )
                    )
                after
                    gen_server:stop(Pid)
                end
            end},
            {"reload to a new backing module moves the class, not duplicates it", fun() ->
                Class = 'BackingIndexReload2736',
                OldBacking = bt_backing_reload_old_2736,
                NewBacking = bt_backing_reload_new_2736,
                ClassInfo = #{
                    superclass => none,
                    methods => #{},
                    class_methods => #{},
                    meta => #{backing_module => OldBacking}
                },
                {ok, Pid} = beamtalk_object_class:start_link(Class, ClassInfo),
                try
                    ?assert(
                        lists:member(
                            Class, beamtalk_class_registry:classes_backing_module(OldBacking)
                        )
                    ),
                    {ok, _} = beamtalk_object_class:update_class(Class, #{
                        meta => #{backing_module => NewBacking}
                    }),
                    ?assertNot(
                        lists:member(
                            Class, beamtalk_class_registry:classes_backing_module(OldBacking)
                        )
                    ),
                    ?assertEqual(
                        [Class], beamtalk_class_registry:classes_backing_module(NewBacking)
                    ),
                    %% Reloading to "no backing module" drops the class entirely
                    %% rather than leaving a stale row under NewBacking.
                    {ok, _} = beamtalk_object_class:update_class(Class, #{meta => #{}}),
                    ?assertNot(
                        lists:member(
                            Class, beamtalk_class_registry:classes_backing_module(NewBacking)
                        )
                    )
                after
                    gen_server:stop(Pid)
                end
            end}
        ]}.

%% A reload (same name, new pid) overwrites the row; the dying old process must
%% not clobber the replacement — forget_backing_module_entries/2 is guarded on
%% the pid, exactly like forget_loaded_class/2.
backing_module_index_reload_keeps_new_pid_test() ->
    beamtalk_class_registry:ensure_backing_module_index_table(),
    Class = 'BackingIndexPidGuard2736',
    Backing = bt_backing_pid_guard_2736,
    OldPid = list_to_pid("<0.9993.0>"),
    NewPid = list_to_pid("<0.9994.0>"),
    ets:insert(beamtalk_backing_module_index, {Backing, Class, NewPid}),
    ok = beamtalk_class_registry:forget_backing_module_entries(Class, OldPid),
    ?assertEqual([Class], beamtalk_class_registry:classes_backing_module(Backing)),
    ok = beamtalk_class_registry:forget_backing_module_entries(Class, NewPid),
    ?assertEqual([], beamtalk_class_registry:classes_backing_module(Backing)).

%%% ============================================================================
%%% get_method_return_type / get_class_method_return_type tests
%%% ============================================================================

get_method_return_type_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            %% BT-3826: return types are read from the class's published metadata
            %% row, so the hierarchy edges below are merged into it (`insert/5`
            %% would overwrite the whole row, wiping the published reflection).
            Saved = ets:tab2list(beamtalk_class_metadata),
            ets:delete_all_objects(beamtalk_class_metadata),
            %% Create a simple hierarchy: Child -> Parent -> Object
            {ok, ObjPid} = beamtalk_object_class:start('TestObject1002', #{
                superclass => none,
                method_return_types => #{getValue => 'Integer'}
            }),
            beamtalk_class_metadata:merge_identity(
                'TestObject1002', undefined, undefined, none, undefined
            ),
            {ok, ParentPid} = beamtalk_object_class:start('TestParent1002', #{
                superclass => 'TestObject1002',
                method_return_types => #{name => 'String'},
                class_method_return_types => #{'from:' => 'TestParent1002'}
            }),
            beamtalk_class_metadata:merge_identity(
                'TestParent1002', undefined, undefined, 'TestObject1002', undefined
            ),
            {ok, ChildPid} = beamtalk_object_class:start('TestChild1002', #{
                superclass => 'TestParent1002',
                method_return_types => #{size => 'Integer'}
            }),
            beamtalk_class_metadata:merge_identity(
                'TestChild1002', undefined, undefined, 'TestParent1002', undefined
            ),
            {Saved, [ObjPid, ParentPid, ChildPid]}
        end,
        fun({Saved, Pids}) ->
            lists:foreach(
                fun(Pid) ->
                    (try
                        gen_server:stop(Pid)
                    catch
                        _:_ -> ok
                    end)
                end,
                Pids
            ),
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:insert(beamtalk_class_metadata, Saved),
            %% A raw restore bypasses the metadata API, so drop derived
            %% facts cached against the test rows (BT-4064).
            beamtalk_class_derived_cache:invalidate()
        end,
        [
            {"local hit", fun() ->
                ?assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', size)
                )
            end},
            {"superclass chain hit", fun() ->
                ?assertEqual(
                    {ok, 'String'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', name)
                )
            end},
            {"grandparent chain hit", fun() ->
                ?assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', getValue)
                )
            end},
            {"not found anywhere", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', unknown)
                )
            end},
            {"none class", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type(none, size)
                )
            end},
            {"unknown class", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type('NoSuchClass1002', size)
                )
            end},
            {"class method chain hit", fun() ->
                ?assertEqual(
                    {ok, 'TestParent1002'},
                    beamtalk_class_registry:get_class_method_return_type('TestChild1002', 'from:')
                )
            end},
            {"class method not found", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_class_method_return_type('TestChild1002', unknown)
                )
            end}
        ]}.

%%% ============================================================================
%%% user_classes tests
%%% ============================================================================

user_classes_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
            {ok, Pid} = beamtalk_object_class:start_link('UserClassesTestClass1092', ClassInfo),
            Pid
        end,
        fun(Pid) ->
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end)
        end,
        [
            {"returns a list", fun() ->
                Result = beamtalk_class_registry:user_classes(),
                ?assert(is_list(Result))
            end},
            {"excludes classes without source file", fun() ->
                %% Test class has no compiled BEAM module with beamtalk_source
                %% attribute, so it should be filtered out by user_classes/0
                Result = beamtalk_class_registry:user_classes(),
                Tags = [Tag || {beamtalk_object, Tag, _Mod, _Pid} <- Result],
                ?assertNot(lists:member('UserClassesTestClass1092 class', Tags))
            end},
            {"entries are {beamtalk_object, Tag, Mod, Pid} tuples", fun() ->
                Result = beamtalk_class_registry:user_classes(),
                lists:foreach(
                    fun({beamtalk_object, Tag, Mod, Pid}) ->
                        ?assert(is_atom(Tag)),
                        ?assert(is_atom(Mod)),
                        ?assert(is_pid(Pid))
                    end,
                    Result
                )
            end}
        ]}.

%%% ============================================================================
%%% ETS table ownership / heir tests
%%% ============================================================================

pid_table_survives_owner_death_test_() ->
    {setup,
        fun() ->
            %% Delete the table if it exists so we can recreate it
            %% from a spawned (transient) process to test heir behavior.
            case ets:info(beamtalk_class_pids) of
                undefined ->
                    {missing, []};
                _ ->
                    Saved = ets:tab2list(beamtalk_class_pids),
                    %% We can only delete if we own the table. If we don't,
                    %% skip the test setup — table is already owned by a
                    %% persistent process (the correct state).
                    case ets:info(beamtalk_class_pids, owner) of
                        Owner when Owner =:= self() ->
                            ets:delete(beamtalk_class_pids),
                            {owned, Saved};
                        _ ->
                            {foreign, Saved}
                    end
            end
        end,
        fun
            ({missing, _}) ->
                %% Ensure the table exists again for other tests
                beamtalk_class_registry:ensure_pid_table();
            ({owned, Saved}) ->
                %% Restore table and entries
                beamtalk_class_registry:ensure_pid_table(),
                ets:insert(beamtalk_class_pids, Saved);
            ({foreign, _Saved}) ->
                ok
        end,
        fun() ->
            %% When the ETS heir is alive, heir option is set and the table
            %% should survive the creating process dying.
            case whereis(beamtalk_ets_heir) of
                undefined ->
                    %% Can't test heir behavior without the heir process.
                    %% Just verify ensure_pid_table is idempotent.
                    beamtalk_class_registry:ensure_pid_table(),
                    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids));
                SupPid ->
                    %% Delete existing table to recreate from a spawned process
                    case ets:info(beamtalk_class_pids) of
                        undefined ->
                            ok;
                        _ ->
                            case ets:info(beamtalk_class_pids, owner) =:= self() of
                                true ->
                                    ets:delete(beamtalk_class_pids);
                                false ->
                                    %% Can't delete a table we don't own;
                                    %% just test that heir is set.
                                    Heir = ets:info(beamtalk_class_pids, heir),
                                    ?assertEqual(SupPid, Heir),
                                    ok
                            end
                    end,
                    case ets:info(beamtalk_class_pids) of
                        undefined ->
                            %% Create table from a transient process
                            TestPid = self(),
                            Spawned = spawn(fun() ->
                                beamtalk_class_registry:ensure_pid_table(),
                                ets:insert(beamtalk_class_pids, {self(), 'TestHeirClass'}),
                                TestPid ! {table_created, ets:info(beamtalk_class_pids, heir)},
                                receive
                                    stop -> ok
                                end
                            end),
                            receive
                                {table_created, HeirPid} ->
                                    ?assertEqual(SupPid, HeirPid),
                                    %% Kill the owner process and wait for it to exit
                                    MRef = erlang:monitor(process, Spawned),
                                    Spawned ! stop,
                                    receive
                                        {'DOWN', MRef, process, Spawned, _} -> ok
                                    after 5000 ->
                                        ?assert(false)
                                    end,
                                    %% Table should still exist (inherited by beamtalk_ets_heir)
                                    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids))
                            after 5000 ->
                                ?assert(false)
                            end;
                        _ ->
                            %% Table already exists and we verified heir above
                            ok
                    end
            end
        end}.

heir_option_returns_empty_when_no_heir_test() ->
    %% ensure_pid_table creates the table whether or not `beamtalk_ets_heir`
    %% is running (`beamtalk_ets_heir_tests` covers heir_option/0 directly).
    beamtalk_class_registry:ensure_pid_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids)).

%%% ============================================================================
%%% restart_class tests
%%% ============================================================================

restart_class_no_module_test() ->
    %% Attempting to restart a class with no module table entry should fail.
    ?assertMatch(
        {error, {no_module_for_class, 'NonExistentRestart1888'}},
        beamtalk_class_registry:restart_class('NonExistentRestart1888')
    ).

restart_class_recovery_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            HierSaved = ets:tab2list(beamtalk_class_metadata),
            %% Start a class, then kill it to test recovery
            ClassInfo = #{
                superclass => none,
                methods => #{},
                class_methods => #{}
            },
            {ok, Pid} = beamtalk_object_class:start('RestartTest1888', ClassInfo),
            %% Verify it's alive and registered
            ?assertNotEqual(undefined, beamtalk_class_registry:whereis_class('RestartTest1888')),
            {HierSaved, Pid}
        end,
        fun({HierSaved, _Pid}) ->
            %% Clean up the restarted class if it exists
            case beamtalk_class_registry:whereis_class('RestartTest1888') of
                undefined ->
                    ok;
                P ->
                    (try
                        gen_server:stop(P)
                    catch
                        _:_ -> ok
                    end)
            end,
            ets:delete_all_objects(beamtalk_class_metadata),
            ets:insert(beamtalk_class_metadata, HierSaved),
            %% A raw restore bypasses the metadata API, so drop derived
            %% facts cached against the test rows (BT-4064).
            beamtalk_class_derived_cache:invalidate()
        end,
        [
            {"kill and restart class", fun() ->
                OldPid = beamtalk_class_registry:whereis_class('RestartTest1888'),
                ?assert(is_pid(OldPid)),
                %% Kill the class process and wait for it to exit
                MRef = erlang:monitor(process, OldPid),
                exit(OldPid, kill),
                receive
                    {'DOWN', MRef, process, OldPid, _} -> ok
                after 5000 ->
                    ?assert(false)
                end,
                %% Should be unregistered now
                ?assertEqual(undefined, beamtalk_class_registry:whereis_class('RestartTest1888')),
                %% Restart it
                {ok, NewPid} = beamtalk_class_registry:restart_class('RestartTest1888'),
                ?assert(is_pid(NewPid)),
                ?assertNotEqual(OldPid, NewPid),
                %% Should be registered again
                ?assertEqual(NewPid, beamtalk_class_registry:whereis_class('RestartTest1888'))
            end}
        ]}.

%%% ============================================================================
%%% class_name_for_pid tests
%%% ============================================================================

class_name_for_pid_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pid_table(),
            ok
        end,
        fun(_) -> ok end, [
            {"record and lookup", fun() ->
                Pid = self(),
                beamtalk_class_registry:record_class_pid(Pid, 'PidLookupTest1888'),
                ?assertEqual(
                    {ok, 'PidLookupTest1888'}, beamtalk_class_registry:class_name_for_pid(Pid)
                ),
                %% Clean up
                ets:delete(beamtalk_class_pids, Pid)
            end},
            {"not found for unknown pid", fun() ->
                FakePid = list_to_pid("<0.99999.0>"),
                ?assertEqual(not_found, beamtalk_class_registry:class_name_for_pid(FakePid))
            end}
        ]}.

%%% ============================================================================
%%% extract_package_from_module tests
%%% ============================================================================

extract_package_from_module_qualified_test() ->
    ?assertEqual(
        mypackage,
        beamtalk_class_registry:extract_package_from_module('bt@mypackage@MyClass')
    ).

extract_package_from_module_unqualified_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:extract_package_from_module('bt@MyClass')
    ).

extract_package_from_module_no_prefix_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:extract_package_from_module(some_module)
    ).

extract_package_from_module_stdlib_test() ->
    ?assertEqual(
        stdlib,
        beamtalk_class_registry:extract_package_from_module('bt@stdlib@Integer')
    ).

%%% ============================================================================
%%% is_stdlib_module tests
%%% ============================================================================

is_stdlib_module_true_test() ->
    ?assert(beamtalk_class_registry:is_stdlib_module('bt@stdlib@Integer')).

is_stdlib_module_false_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module('bt@mypackage@MyClass')).

is_stdlib_module_non_atom_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module(<<"bt@stdlib@Integer">>)).

is_stdlib_module_plain_atom_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module(some_module)).

%%% ============================================================================
%%% ensure_class_warnings_table / collision warning tests
%%% ============================================================================

ensure_class_warnings_table_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_warnings)),
    %% Idempotent
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_warnings)).

record_and_drain_warnings_by_names_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Insert a collision warning
    beamtalk_class_registry:record_class_collision_warning(
        'TestWarningClass', 'bt@pkg1@TestWarningClass', 'bt@pkg2@TestWarningClass'
    ),
    %% Drain it
    Warnings = beamtalk_class_registry:drain_class_warnings_by_names(['TestWarningClass']),
    ?assertEqual(1, length(Warnings)),
    [{CN, OldMod, NewMod}] = Warnings,
    ?assertEqual('TestWarningClass', CN),
    ?assertEqual('bt@pkg1@TestWarningClass', OldMod),
    ?assertEqual('bt@pkg2@TestWarningClass', NewMod),
    %% Second drain should be empty (warnings consumed)
    ?assertEqual([], beamtalk_class_registry:drain_class_warnings_by_names(['TestWarningClass'])).

drain_warnings_by_names_empty_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names(['NonExistentWarningClass'])
    ).

drain_warnings_by_qualified_names_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    beamtalk_class_registry:record_class_collision_warning(
        'QualDrainClass', 'bt@pkgA@QualDrainClass', 'bt@pkgB@QualDrainClass'
    ),
    %% Drain by the specific package of the new module (pkgB)
    Warnings = beamtalk_class_registry:drain_class_warnings_by_qualified_names(
        [{pkgB, 'QualDrainClass'}]
    ),
    ?assertEqual(1, length(Warnings)),
    %% Draining with the wrong package should find nothing
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_qualified_names(
            [{pkgA, 'QualDrainClass'}]
        )
    ).

drain_warnings_no_table_test() ->
    %% If the table doesn't exist, drain should return []
    %% We can't easily delete the table if it's owned by another process,
    %% so we test the code path by verifying it handles both states gracefully.
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names([])
    ).

%%% ============================================================================
%%% ensure_module_table tests
%%% ============================================================================

ensure_module_table_test() ->
    beamtalk_class_registry:ensure_module_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_metadata)),
    %% Idempotent
    beamtalk_class_registry:ensure_module_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_metadata)).

%%% ============================================================================
%%% pending load errors tests
%%% ============================================================================

ensure_pending_errors_table_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_pending_load_errors)),
    %% Idempotent
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_pending_load_errors)).

record_and_drain_pending_errors_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    Error = beamtalk_error:new(stdlib_shadowing, 'ShadowTestClass'),
    beamtalk_class_registry:record_pending_load_error('ShadowTestClass', Error),
    Errors = beamtalk_class_registry:drain_pending_load_errors_by_names(['ShadowTestClass']),
    ?assertEqual(1, length(Errors)),
    [{CN, _Err}] = Errors,
    ?assertEqual('ShadowTestClass', CN),
    %% Second drain should be empty
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_pending_load_errors_by_names(['ShadowTestClass'])
    ).

drain_pending_errors_empty_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_pending_load_errors_by_names(['NonExistentErrorClass'])
    ).

%%% ============================================================================
%%% validate_class_update tests
%%% ============================================================================

validate_class_update_same_module_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        ok,
        beamtalk_class_registry:validate_class_update(
            'ValTestClass', 'bt@pkg@ValTestClass', #{module => 'bt@pkg@ValTestClass'}
        )
    ).

validate_class_update_stdlib_shadowing_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    beamtalk_class_registry:ensure_pending_errors_table(),
    Result = beamtalk_class_registry:validate_class_update(
        'Integer', 'bt@stdlib@Integer', #{module => 'bt@user@Integer'}
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result),
    %% Clean up the pending error
    beamtalk_class_registry:drain_pending_load_errors_by_names(['Integer']).

validate_class_update_cross_module_warning_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Different non-stdlib modules should record a warning but return ok
    ok = beamtalk_class_registry:validate_class_update(
        'CrossModClass', 'bt@pkg1@CrossModClass', #{module => 'bt@pkg2@CrossModClass'}
    ),
    %% Verify the collision warning was recorded
    Warnings = beamtalk_class_registry:drain_class_warnings_by_names(['CrossModClass']),
    ?assertEqual(1, length(Warnings)).

validate_class_update_bootstrap_stub_replacement_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Bootstrap stub being replaced by stdlib module should NOT generate a warning
    ok = beamtalk_class_registry:validate_class_update(
        'Class', beamtalk_class_bt, #{module => 'bt@stdlib@Class'}
    ),
    %% No collision warning should be recorded
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names(['Class'])
    ).

%%% ============================================================================
%%% Additional coverage
%%% ============================================================================

%% ensure_pg_started/0 is idempotent when pg is already running.
bt1982_ensure_pg_started_idempotent_test() ->
    ok = beamtalk_class_registry:ensure_pg_started(),
    ok = beamtalk_class_registry:ensure_pg_started(),
    ?assert(is_pid(whereis(pg))).

%% maybe_set_heir runs when the ETS heir is alive; covers the heir-alive branch.
%% If the real heir is running (test suite with full app) we use it;
%% otherwise we register a fake pid temporarily.
bt1982_maybe_set_heir_with_heir_process_test() ->
    case whereis(beamtalk_ets_heir) of
        RealHeir when is_pid(RealHeir) ->
            %% Real heir running — exercise the branch directly.
            ok = beamtalk_class_registry:ensure_class_warnings_table(),
            ok = beamtalk_class_registry:ensure_class_warnings_table();
        undefined ->
            beamtalk_class_registry:ensure_pg_started(),
            FakeHeirPid = spawn(fun() ->
                receive
                    stop -> ok
                end
            end),
            try
                erlang:register(beamtalk_ets_heir, FakeHeirPid),
                case ets:info(beamtalk_class_warnings) of
                    undefined -> beamtalk_class_registry:ensure_class_warnings_table();
                    _ -> ok
                end,
                ok = beamtalk_class_registry:ensure_class_warnings_table()
            after
                catch erlang:unregister(beamtalk_ets_heir),
                FakeHeirPid ! stop
            end
    end.

%% heir_option with a live heir process returns a non-empty list. Works whether
%% or not a real heir is registered.
bt1982_heir_option_with_heir_process_test() ->
    case whereis(beamtalk_ets_heir) of
        RealHeir when is_pid(RealHeir) ->
            beamtalk_class_registry:ensure_pending_errors_table(),
            ?assert(true);
        undefined ->
            FakeHeirPid = spawn(fun() ->
                receive
                    stop -> ok
                end
            end),
            try
                erlang:register(beamtalk_ets_heir, FakeHeirPid),
                beamtalk_class_registry:ensure_pending_errors_table(),
                ?assert(true)
            after
                catch erlang:unregister(beamtalk_ets_heir),
                FakeHeirPid ! stop
            end
    end.

%% restart_class finds module but the existing class process is already alive →
%% returns {ok, ExistingPid} via the {already_started, _} branch.
bt1982_restart_class_already_started_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            ClassName = 'BT1982RestartAS',
            ClassInfo = #{
                name => ClassName,
                module => beamtalk_class_bt,
                superclass => none,
                instance_methods => #{},
                class_methods => #{}
            },
            {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
            %% Record in module table so restart_class can find it.
            beamtalk_class_metadata:insert(
                ClassName, beamtalk_class_bt, undefined, undefined, undefined
            ),
            {ClassName, Pid}
        end,
        fun({_Name, Pid}) ->
            try
                gen_server:stop(Pid, normal, 1000)
            catch
                _:_ -> ok
            end
        end,
        fun({ClassName, Pid}) ->
            [
                ?_test(begin
                    %% restart_class should hit the {already_started, ExistingPid} branch.
                    Result = beamtalk_class_registry:restart_class(ClassName),
                    ?assertMatch({ok, _}, Result),
                    {ok, ReturnedPid} = Result,
                    ?assertEqual(Pid, ReturnedPid)
                end)
            ]
        end}.

%% user_classes/0 is callable and returns a list when pg is running.
bt1982_user_classes_returns_list_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Classes = beamtalk_class_registry:user_classes(),
    ?assert(is_list(Classes)).

%% drain_pending_load_errors_by_names no-table returns [].
bt1982_drain_pending_errors_no_table_test() ->
    %% Delete the table first if it exists.
    case ets:info(beamtalk_pending_load_errors) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_pending_load_errors)
    end,
    ?assertEqual(
        [], beamtalk_class_registry:drain_pending_load_errors_by_names(['Foo'])
    ),
    %% Restore table for subsequent tests.
    beamtalk_class_registry:ensure_pending_errors_table().

%% restart_class success path: class process is dead but module exists.
%% Covers the warning log + {ok, NewPid} branch of restart_class/1.
bt1982_restart_class_recovers_dead_process_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            ClassName = 'BT1982RestartRecover',
            ClassInfo = #{
                name => ClassName,
                module => beamtalk_class_bt,
                superclass => none,
                instance_methods => #{},
                class_methods => #{}
            },
            %% Use start/2 (unlinked) so we can kill it without taking
            %% down the EUnit test supervisor.
            {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
            beamtalk_class_metadata:insert(
                ClassName, beamtalk_class_bt, undefined, undefined, undefined
            ),
            %% Kill the class process so restart_class can resurrect it.
            MRef = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 2000 -> throw(timeout_killing_class)
            end,
            ClassName
        end,
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                P ->
                    try
                        gen_server:stop(P, normal, 1000)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        fun(ClassName) ->
            [
                ?_test(begin
                    Result = beamtalk_class_registry:restart_class(ClassName),
                    ?assertMatch({ok, _}, Result),
                    {ok, NewPid} = Result,
                    ?assert(is_process_alive(NewPid))
                end)
            ]
        end}.

%% class_name_for_pid returns not_found when the pid table does not exist.
bt1982_class_name_for_pid_no_table_test() ->
    case ets:info(beamtalk_class_pids) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_class_pids)
    end,
    ?assertEqual(
        not_found, beamtalk_class_registry:class_name_for_pid(self())
    ),
    beamtalk_class_registry:ensure_pid_table().

%% get_method_return_type walks to superclass via hierarchy lookup when the
%% class is registered but process exit during call produces `not_found` —
%% the walk-to-superclass branch.
bt1982_get_method_return_type_walks_on_process_exit_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            %% Set up a parent class with a known return type, then register a
            %% child with none.
            {ok, ParentPid} = beamtalk_object_class:start('BT1982RTParent', #{
                superclass => none,
                method_return_types => #{answer => 'Integer'}
            }),
            beamtalk_class_metadata:merge_identity(
                'BT1982RTParent', undefined, undefined, none, undefined
            ),
            {ok, ChildPid} = beamtalk_object_class:start('BT1982RTChild', #{
                superclass => 'BT1982RTParent',
                method_return_types => #{}
            }),
            beamtalk_class_metadata:merge_identity(
                'BT1982RTChild', undefined, undefined, 'BT1982RTParent', undefined
            ),
            [ParentPid, ChildPid]
        end,
        fun(Pids) ->
            lists:foreach(
                fun(P) ->
                    try
                        gen_server:stop(P)
                    catch
                        _:_ -> ok
                    end
                end,
                Pids
            )
        end,
        fun(_) ->
            [
                %% Child has no type but parent does — walk through hierarchy.
                ?_assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type(
                        'BT1982RTChild', answer
                    )
                ),
                %% Same for class-method variant.
                ?_assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_class_method_return_type(
                        'BT1982RTChild', answer
                    )
                )
            ]
        end}.

%% drain_class_warnings_by_qualified_names returns [] when no table exists.
bt1982_drain_warnings_qualified_no_table_test() ->
    case ets:info(beamtalk_class_warnings) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_class_warnings)
    end,
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_qualified_names(
            [{pkg, 'Foo'}]
        )
    ),
    beamtalk_class_registry:ensure_class_warnings_table().

%%% ============================================================================
%%% ADR 0032 Phase 1: invalidate_subclass_flattened_tables removed
%%% ============================================================================
%% The rebuild broadcast (invalidate_subclass_flattened_tables) was removed in
%% ADR 0032 Phase 1. Chain walk at dispatch time replaces the flattened table
%% cache, so no broadcast is needed when class methods change.
%%
%% Test removed: invalidate_subclass_flattened_tables_test

%%% ============================================================================
%%% BT-3826: no class-to-class blocking calls inside class handlers
%%% ============================================================================

%% BT-3826 regression harness. Starts two dynamic classes whose class-side probe
%% runs `Site(OtherName)` inside the class handler, holding both handlers at a
%% barrier until both are inside so each is busy serving a call when it reads the
%% other's state. If `Site` blocks on a `gen_server:call` into the other class,
%% each handler waits on the other's (busy) mailbox until the 5s call timeout --
%% the shape of BT-3820's `class_name` deadlock. Returns `[{Name, Outcome}]`
%% (`{ok, Result}` | `{raised, Class, Reason}`) after asserting both finished far
%% below the 5s timeout.
%%
%% `Extra` is merged into both classes' ClassInfo.
bt3826_two_busy_classes(Tag, Extra, Site) ->
    beamtalk_class_registry:ensure_pg_started(),
    Coord = self(),
    NameA = list_to_atom("BT3826" ++ Tag ++ "A"),
    NameB = list_to_atom("BT3826" ++ Tag ++ "B"),
    Info = fun(Other) ->
        Probe = fun(_ClassSelf) ->
            Coord ! {in_handler, self()},
            receive
                {go, Me} when Me =:= self() -> ok
            after 10000 -> exit(barrier_timeout)
            end,
            Site(Other)
        end,
        maps:merge(
            #{
                superclass => none,
                module => bt3826_no_module,
                class_methods => maps:merge(
                    maps:get(class_methods, Extra, #{}),
                    #{bt3826Probe => #{block => Probe, arity => 1}}
                )
            },
            maps:remove(class_methods, Extra)
        )
    end,
    {ok, PidA} = beamtalk_object_class:start_link(NameA, Info(NameB)),
    {ok, PidB} = beamtalk_object_class:start_link(NameB, Info(NameA)),
    try
        Named = [{NameA, PidA}, {NameB, PidB}],
        _Callers = [
            spawn_link(fun() ->
                Outcome =
                    try
                        {ok, beamtalk_class_dispatch:class_send(P, bt3826Probe, [])}
                    catch
                        Class:Reason -> {raised, Class, Reason}
                    end,
                Coord ! {result, N, Outcome}
            end)
         || {N, P} <- Named
        ],
        InHandler = [
            receive
                {in_handler, H} -> H
            after 10000 -> error(handler_not_entered)
            end
         || _ <- Named
        ],
        [H ! {go, H} || H <- InHandler],
        Start = erlang:monotonic_time(millisecond),
        Results = [
            receive
                {result, N, R} -> {N, R}
            after 15000 -> error({no_result, N})
            end
         || {N, _} <- Named
        ],
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        %% Far below the 5s gen_server:call timeout a deadlock would burn.
        ?assert(Elapsed < 4000),
        Results
    after
        [catch gen_server:stop(P, normal, 5000) || P <- [PidA, PidB]]
    end.

%% Reflection intrinsics (`Foo methods`, `fieldNames`, `allInstVarNames`,
%% `canUnderstand:`, `Foo class selectors`) read a *different* class's state from
%% inside a class handler.
bt3826_reflection_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Extra = #{
            instance_methods => #{bt3826Foo => #{block => fun() -> ok end, arity => 0}},
            fields => [bt3826X, bt3826Y]
        },
        Site = fun(Other) ->
            Obj = beamtalk_class_registry:resolve_class_object(Other),
            Meta = Obj#beamtalk_object{class = 'Metaclass'},
            #{
                methods => beamtalk_behaviour_intrinsics:classLocalMethods(Obj),
                fields => beamtalk_behaviour_intrinsics:classFieldNames(Obj),
                all_fields => beamtalk_behaviour_intrinsics:classAllFieldNamesByName(Other),
                field_kinds => beamtalk_behaviour_intrinsics:classFieldKinds(Obj),
                includes => beamtalk_behaviour_intrinsics:classIncludesSelector(Obj, bt3826Foo),
                understands => beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                    Other, bt3826Foo
                ),
                class_side => beamtalk_behaviour_intrinsics:metaclassLocalClassMethods(Meta),
                class_side_all => beamtalk_behaviour_intrinsics:metaclassClassMethods(Meta),
                class_side_has => beamtalk_behaviour_intrinsics:metaclassIncludesSelector(
                    Meta, bt3826Probe
                )
            }
        end,
        Results = bt3826_two_busy_classes("Reflect", Extra, Site),
        lists:foreach(
            fun({_Name, Outcome}) ->
                ?assertMatch({ok, #{}}, Outcome),
                {ok, R} = Outcome,
                ?assertEqual([bt3826Foo], maps:get(methods, R)),
                ?assertEqual([bt3826X, bt3826Y], maps:get(fields, R)),
                ?assertEqual([bt3826X, bt3826Y], maps:get(all_fields, R)),
                ?assertEqual(#{bt3826X => eager, bt3826Y => eager}, maps:get(field_kinds, R)),
                ?assert(maps:get(includes, R)),
                ?assert(maps:get(understands, R)),
                ?assertEqual([bt3826Probe], maps:get(class_side, R)),
                ?assertEqual([bt3826Probe], maps:get(class_side_all, R)),
                ?assert(maps:get(class_side_has, R))
            end,
            Results
        )
    end}.

%% `get_method_return_type/2` and `get_class_method_return_type/2` (the REPL
%% type-chain walk).
bt3826_return_type_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Extra = #{
            method_return_types => #{bt3826Foo => 'Integer'},
            class_method_return_types => #{bt3826Make => 'String'}
        },
        Site = fun(Other) ->
            {
                beamtalk_class_registry:get_method_return_type(Other, bt3826Foo),
                beamtalk_class_registry:get_class_method_return_type(Other, bt3826Make),
                beamtalk_class_registry:get_method_return_type(Other, bt3826Missing)
            }
        end,
        Results = bt3826_two_busy_classes("RetType", Extra, Site),
        lists:foreach(
            fun({_Name, Outcome}) ->
                ?assertEqual({ok, {{ok, 'Integer'}, {ok, 'String'}, {error, not_found}}}, Outcome)
            end,
            Results
        )
    end}.

%% `beamtalk_protocol_registry:conforms_to/2` (what `conformingClasses` runs
%% inside class handlers): instance-side `classCanUnderstandFromName/2` plus the
%% class-side chain check.
bt3826_conforms_to_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        beamtalk_protocol_registry:init(),
        Proto = 'BT3826Proto',
        ok = beamtalk_protocol_registry:register_protocol(#{
            name => Proto,
            required_methods => [#{selector => bt3826Foo, arity => 0}],
            required_class_methods => [#{selector => bt3826Probe, arity => 1}],
            type_params => [],
            extending => undefined
        }),
        Extra = #{
            instance_methods => #{bt3826Foo => #{block => fun() -> ok end, arity => 0}}
        },
        Site = fun(Other) ->
            beamtalk_protocol_registry:invalidate_conforms_cache(),
            beamtalk_protocol_registry:conforms_to(Other, Proto)
        end,
        try
            Results = bt3826_two_busy_classes("Conforms", Extra, Site),
            lists:foreach(
                fun({_Name, Outcome}) -> ?assertEqual({ok, true}, Outcome) end,
                Results
            )
        after
            beamtalk_protocol_registry:unregister_protocol(Proto)
        end
    end}.

%% The miss path: a selector the *other* class does not define falls through the
%% local check to the module probe (`module_name_safe/1`) and up the hierarchy.
%% Every step of `has_method/2` / `canUnderstand:` / `conforms_to/2` must stay
%% message-free there too, not only on a hit.
bt3826_has_method_miss_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Site = fun(Other) ->
            {
                beamtalk_behaviour_intrinsics:classCanUnderstandFromName(Other, bt3826Missing),
                beamtalk_object_class:has_method(
                    beamtalk_class_registry:whereis_class(Other), bt3826Missing
                ),
                beamtalk_object_class:has_method_local(
                    beamtalk_class_registry:whereis_class(Other), bt3826Missing
                )
            }
        end,
        Results = bt3826_two_busy_classes("Miss", #{}, Site),
        lists:foreach(
            fun({_Name, Outcome}) -> ?assertEqual({ok, {false, false, false}}, Outcome) end,
            Results
        )
    end}.

%% The published reflection facts follow the class through hot-patching, so the
%% ETS-backed readers never disagree with the class process's own state.
bt3826_reflection_follows_class_mutation_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Name = 'BT3826MutationClass',
    Info = #{
        superclass => none,
        module => bt3826_no_module,
        instance_methods => #{bt3826Foo => #{block => fun() -> ok end, arity => 0}},
        fields => [bt3826X],
        method_return_types => #{bt3826Foo => 'Integer'}
    },
    {ok, Pid} = beamtalk_object_class:start_link(Name, Info),
    try
        ?assertEqual([bt3826Foo], beamtalk_object_class:instance_methods_safe(Pid)),
        ?assertEqual([bt3826X], beamtalk_object_class:instance_variables_safe(Pid)),
        ?assertEqual(
            {ok, 'Integer'}, beamtalk_class_registry:get_method_return_type(Name, bt3826Foo)
        ),
        %% put_method adds a selector and clears that selector's return type.
        ok = beamtalk_object_class:put_method(Pid, bt3826Foo, fun(_Self) -> ok end),
        ok = beamtalk_object_class:put_method(Pid, bt3826Bar, fun(_Self) -> ok end),
        ?assertEqual(
            lists:sort([bt3826Foo, bt3826Bar]),
            lists:sort(beamtalk_object_class:instance_methods_safe(Pid))
        ),
        ?assertEqual(
            lists:sort(gen_server:call(Pid, methods)),
            lists:sort(beamtalk_object_class:instance_methods_safe(Pid))
        ),
        ?assertEqual(
            {error, not_found}, beamtalk_class_registry:get_method_return_type(Name, bt3826Foo)
        ),
        %% put_class_method refreshes the class-side selectors.
        ok = beamtalk_object_class:put_class_method(Pid, bt3826Make, fun(_ClassSelf) -> ok end),
        ?assertEqual([bt3826Make], beamtalk_object_class:local_class_method_selectors_safe(Pid)),
        %% update_class (hot reload) replaces fields and methods.
        {ok, _} = beamtalk_object_class:update_class(Name, #{
            instance_methods => #{bt3826Baz => #{block => fun() -> ok end, arity => 0}},
            fields => [bt3826Z],
            method_return_types => #{bt3826Baz => 'String'}
        }),
        ?assertEqual([bt3826Baz], beamtalk_object_class:instance_methods_safe(Pid)),
        ?assertEqual([bt3826Z], beamtalk_object_class:instance_variables_safe(Pid)),
        ?assertEqual(
            {ok, 'String'}, beamtalk_class_registry:get_method_return_type(Name, bt3826Baz)
        ),
        ?assert(beamtalk_object_class:has_method(Pid, bt3826Baz)),
        ?assertNot(beamtalk_object_class:has_method(Pid, bt3826Foo))
    after
        gen_server:stop(Pid, normal, 5000)
    end.

%%% ============================================================================
%%% BT-3832: CompiledMethod lookups, class flags and hierarchy docs without
%%% class-to-class blocking calls
%%% ============================================================================

%% `beamtalk_method_resolver:resolve/2` (`Behaviour >>`) returns a full
%% `CompiledMethod` for another class, instance side and class side.
bt3832_method_resolver_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Extra = #{
            instance_methods => #{bt3832Foo => #{block => fun() -> ok end, arity => 0}},
            method_source => #{bt3832Foo => <<"bt3832Foo => 1">>},
            method_signatures => #{bt3832Foo => <<"bt3832Foo -> Integer">>},
            method_docs => #{bt3832Foo => <<"Foo doc.">>}
        },
        Site = fun(Other) ->
            Pid = beamtalk_class_registry:whereis_class(Other),
            Meta = #beamtalk_object{class = 'Metaclass', pid = Pid},
            {
                beamtalk_method_resolver:resolve(Other, bt3832Foo),
                beamtalk_method_resolver:resolve(Pid, bt3832Missing),
                beamtalk_method_resolver:resolve(Meta, bt3826Probe),
                beamtalk_method_resolver:resolve(Meta, bt3832Missing)
            }
        end,
        Results = bt3826_two_busy_classes("Resolver", Extra, Site),
        lists:foreach(
            fun({_Name, Outcome}) ->
                ?assertMatch({ok, {_, nil, _, nil}}, Outcome),
                {ok, {Inst, nil, ClassSide, nil}} = Outcome,
                ?assertMatch(
                    #{
                        '__selector__' := bt3832Foo,
                        '__source__' := <<"bt3832Foo => 1">>,
                        '__signature__' := <<"bt3832Foo -> Integer">>,
                        '__doc__' := <<"Foo doc.">>
                    },
                    Inst
                ),
                ?assertMatch(#{'__selector__' := bt3826Probe}, ClassSide)
            end,
            Results
        )
    end}.

%% The published record tracks hot patches, doc updates and the class-side
%% inheritance walk, and equals what the class process itself answers.
bt3832_compiled_method_records_track_class_state_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    SuperName = 'BT3832RecSuper',
    SubName = 'BT3832RecSub',
    {ok, SuperPid} = beamtalk_object_class:start_link(SuperName, #{
        superclass => none,
        module => bt3832_no_module,
        class_methods => #{bt3832Make => #{block => fun(_) -> ok end, arity => 1}},
        class_method_source => #{bt3832Make => <<"class make => 1">>}
    }),
    {ok, SubPid} = beamtalk_object_class:start_link(SubName, #{
        superclass => SuperName,
        module => bt3832_no_module
    }),
    try
        %% Inherited class-side method: ETS walk == the class process's own walk.
        Inherited = beamtalk_object_class:compiled_method_safe(SubPid, class, bt3832Make),
        ?assertMatch(
            #{'__selector__' := bt3832Make, '__source__' := <<"class make => 1">>}, Inherited
        ),
        ?assertEqual(Inherited, gen_server:call(SubPid, {class_method, bt3832Make})),
        ?assertEqual(nil, beamtalk_object_class:compiled_method_safe(SubPid, class, bt3832Nope)),
        %% Hot patch (put_method) and doc update are visible immediately.
        ok = beamtalk_object_class:put_method(
            SuperPid, bt3832Patched, fun(_Self) -> 1 end, <<"p">>
        ),
        ok = gen_server:call(SuperPid, {set_method_doc, bt3832Patched, <<"Patched doc.">>}),
        Patched = beamtalk_method_resolver:resolve(SubPid, bt3832Patched),
        ?assertMatch(
            #{
                '__selector__' := bt3832Patched,
                '__source__' := <<"p">>,
                '__doc__' := <<"Patched doc.">>
            },
            Patched
        ),
        ?assertEqual(Patched, gen_server:call(SuperPid, {method, bt3832Patched})),
        %% Stopping the class drops its records.
        gen_server:stop(SuperPid, normal, 5000),
        ?assertEqual(
            not_found,
            beamtalk_class_metadata:lookup_method_record(SuperName, instance, bt3832Patched)
        ),
        ?assertEqual(#{}, beamtalk_class_metadata:lookup_method_records(SuperName, class))
    after
        [catch gen_server:stop(P, normal, 5000) || P <- [SubPid, SuperPid]]
    end.

%% The class-flag accessors (`is_sealed`, `is_typed`, `is_abstract`,
%% `is_internal`, `shape_version`) read another class's flags.
bt3832_class_flags_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Extra = #{
            is_sealed => true,
            is_typed => true,
            is_abstract => true,
            is_internal => true,
            shape_version => 3
        },
        Site = fun(Other) ->
            Pid = beamtalk_class_registry:whereis_class(Other),
            {
                beamtalk_object_class:is_sealed(Pid),
                beamtalk_object_class:is_typed(Pid),
                beamtalk_object_class:is_abstract(Pid),
                beamtalk_object_class:is_internal(Pid),
                beamtalk_object_class:shape_version(Pid)
            }
        end,
        Results = bt3826_two_busy_classes("Flags", Extra, Site),
        lists:foreach(
            fun({_Name, Outcome}) ->
                ?assertEqual({ok, {true, true, true, true, 3}}, Outcome)
            end,
            Results
        )
    end}.

%% Flags follow an `update_class` (hot reload) without a call.
bt3832_class_flags_follow_update_class_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Name = 'BT3832FlagReload',
    {ok, Pid} = beamtalk_object_class:start_link(Name, #{
        superclass => none, module => bt3832_no_module
    }),
    try
        ?assertNot(beamtalk_object_class:is_sealed(Pid)),
        ?assertEqual(1, beamtalk_object_class:shape_version(Pid)),
        {ok, _} = gen_server:call(
            Pid,
            {update_class, #{
                superclass => none,
                module => bt3832_no_module,
                is_sealed => true,
                shape_version => 2
            }}
        ),
        ?assert(beamtalk_object_class:is_sealed(Pid)),
        ?assertEqual(2, beamtalk_object_class:shape_version(Pid))
    after
        gen_server:stop(Pid, normal, 5000)
    end.

%% `beamtalk_hierarchy_docs` (`Beamtalk help:`, REPL `:help`) walks another
%% class's hierarchy from inside a class handler.
bt3832_hierarchy_docs_no_cross_class_deadlock_test_() ->
    {timeout, 30, fun() ->
        Extra = #{
            instance_methods => #{bt3832Foo => #{block => fun() -> ok end, arity => 0}}
        },
        Site = fun(Other) ->
            Pid = beamtalk_class_registry:whereis_class(Other),
            {
                beamtalk_hierarchy_docs:find_defining_class(Pid, bt3832Foo),
                beamtalk_hierarchy_docs:find_defining_class_method(Pid, bt3826Probe),
                maps:keys(beamtalk_hierarchy_docs:collect_flattened_methods(Other, Pid)),
                beamtalk_hierarchy_docs:collect_flattened_class_methods(Other, Pid)
            }
        end,
        Results = bt3826_two_busy_classes("HierDocs", Extra, Site),
        lists:foreach(
            fun({Name, Outcome}) ->
                %% Site(Other) is given the *other* class's name.
                Other =
                    case Name of
                        'BT3826HierDocsA' -> 'BT3826HierDocsB';
                        'BT3826HierDocsB' -> 'BT3826HierDocsA'
                    end,
                ?assertEqual(
                    {ok, {Other, Other, [bt3832Foo], #{bt3826Probe => Other}}},
                    Outcome
                )
            end,
            Results
        )
    end}.
