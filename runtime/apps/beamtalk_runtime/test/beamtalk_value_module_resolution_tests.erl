%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_value_module_resolution_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Regression tests for BT-4053: a dynamic send to a Value instance of a
registered class must not reach `code_server`.

`beamtalk_primitive:send/3` on a tagged map resolves the class's module through
`module_for_value/1` (the `bt@stdlib@…` candidate) and `class_name_to_module/1`
(the `bt@…` candidate). For a package-qualified class
(`bt@{package}@{snake_case}`) neither candidate exists, and both used to be
probed with `code:ensure_loaded/1` on every send: two failed `code_server`
round trips (~70-190 µs each), serialised across every process in the node.
Resolution now reads the class's registered module from
`beamtalk_class_metadata` first.

The tests trace `code_server:call/1` (the single entry point for every
`code:ensure_loaded/1` miss) on a worker process that performs the sends, so
any reintroduced probe on the hot path fails them.

BT-4071: a send the class module does not define falls through to
`beamtalk_primitive:runtime_instance_method/2`. That lookup must resolve from
the published method records (cached), never building a `CompiledMethod` or
reading `beamtalk_xref`. `self hasField: #x` used to cost ~15-20 µs, almost
all of it two full copies of `xref_class_gen` to fill a provenance field
nobody read.

`class_of_object_by_name/1` memoises the class object in
`beamtalk_class_derived_cache`, so a warm `p class` makes no registry,
metadata or atom-construction call.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%% A class name whose `bt@pkg_hot_value` / `bt@stdlib@pkg_hot_value`
%% convention modules do not exist, registered with package-qualified modules.
-define(CLASS, 'PkgHotValue').
-define(MOD, 'bt@hotpkg@pkg_hot_value').
-define(MOD2, 'bt@hotpkg2@pkg_hot_value').
-define(MODULE_LESS, 'PkgHotModuleLess').

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok = load_accessor_module(?MOD, x),
    ok = load_accessor_module(?MOD2, y),
    ok.

cleanup(_) ->
    lists:foreach(
        fun(M) ->
            code:purge(M),
            code:delete(M)
        end,
        [?MOD, ?MOD2]
    ).

value_module_resolution_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"package Value send and respondsTo: make no code_server call",
            fun package_value_send_makes_no_code_server_call/0},
        {"redefinition and removal of the class are seen by the next send",
            fun resolution_follows_class_redefinition/0},
        {"module-less class resolves without a code_server call",
            fun module_less_class_makes_no_code_server_call/0},
        {"builder-class new/lookup/has_method_local make no code_server call (BT-4056)",
            fun module_less_class_paths_make_no_code_server_call/0},
        {"warm Value sends do no module-name string work (BT-4064)",
            fun warm_value_send_does_no_string_work/0},
        {"warm `class` builds no atoms and reads no registry (BT-4117)",
            fun warm_class_of_object_builds_no_atoms/0},
        {"exception bit follows a superclass change (BT-4064)",
            fun exception_bit_follows_superclass_change/0},
        {"a cached module that is purged and reloaded is re-derived (BT-4064)",
            fun purged_module_is_rederived/0},
        {"inherited Object send makes no xref or CompiledMethod work (BT-4071)",
            fun inherited_object_send_makes_no_xref_call/0},
        {"fun-backed methods added or replaced at runtime are seen by the next send (BT-4071)",
            fun runtime_method_changes_are_seen_by_next_send/0},
        {"BT-4624: perform: finds fun-backed methods of a module-less class",
            fun perform_finds_runtime_fun_methods/0},
        {"BT-4642: perform: of a fun-backed method relays `^` and script-exit throws",
            fun perform_relays_nlr_and_script_exit_from_runtime_fun/0},
        {"method_origin/3 copies no generation table (BT-4071)",
            fun method_origin_copies_no_generation_table/0},
        {"warm tagged-class dispatch is a per-process memo hit, no ETS read (BT-4623)",
            fun memo_hit_reads_no_ets/0},
        {"memo follows re-registration under another module (BT-4623)",
            fun memo_follows_reregistration/0},
        {"memo follows an ancestor reparented into Exception (BT-4623)",
            fun memo_follows_ancestor_reparent/0},
        {"memo of a purged module is re-derived (BT-4623)",
            fun memo_of_purged_module_is_rederived/0},
        {"an uncacheable answer is not memoised (BT-4623)",
            fun uncacheable_answer_is_not_memoised/0},
        {"without a generation nothing is memoised (BT-4623)", fun no_generation_memoises_nothing/0}
    ]}.

package_value_send_makes_no_code_server_call() ->
    %% Neither naming-convention candidate exists, so probing either one would
    %% be a code_server miss.
    ?assertNot(erlang:module_loaded(beamtalk_module_name:to_module_atom(?CLASS))),
    ?assertNot(erlang:module_loaded(beamtalk_module_name:to_stdlib_module_atom(?CLASS))),
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        {Results, Calls} = code_server_calls_during(fun() ->
            [
                beamtalk_primitive:class_name_to_module(?CLASS),
                beamtalk_primitive:send(Self, getX, []),
                beamtalk_primitive:send(Self, getX, []),
                beamtalk_primitive:responds_to(Self, getX),
                beamtalk_primitive:responds_to(Self, nonexistent)
            ]
        end),
        ?assertEqual([?MOD, 7, 7, true, false], Results),
        ?assertEqual([], Calls)
    after
        beamtalk_test_proc:stop(Pid)
    end.

resolution_follows_class_redefinition() ->
    %% Resolution reads the class's metadata row on every send — there is no
    %% separate cache — so a redefinition that moves the class to another
    %% module (merge_identity/5, the hot-reload write) or the class's removal
    %% (terminate/2 deletes the row) is seen by the very next send.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 1, y => 2},
    try
        ?assertEqual(1, beamtalk_primitive:send(Self, getX, [])),
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD2, [], none, false),
        ?assertEqual(?MOD2, beamtalk_primitive:class_name_to_module(?CLASS)),
        ?assertEqual(2, beamtalk_primitive:send(Self, getX, []))
    after
        beamtalk_test_proc:stop(Pid)
    end,
    ?assertEqual(not_found, beamtalk_class_metadata:lookup_module(?CLASS)),
    ?assertEqual(
        beamtalk_module_name:to_module_atom(?CLASS),
        beamtalk_primitive:class_name_to_module(?CLASS)
    ),
    ?assertBtError(does_not_understand, _, getX, beamtalk_primitive:send(Self, getX, [])).

module_less_class_makes_no_code_server_call() ->
    %% A builder-built class registers its own name as its "module"
    %% (beamtalk_object_class:init/1's default). That is never a loadable
    %% module, so it must not be probed on each send either.
    Pid = start_class(?MODULE_LESS, undefined),
    try
        {Module, Calls} = code_server_calls_during(fun() ->
            beamtalk_primitive:class_name_to_module(?MODULE_LESS)
        end),
        ?assertEqual(?MODULE_LESS, Module),
        ?assertEqual([], Calls)
    after
        beamtalk_test_proc:stop(Pid)
    end.

module_less_class_paths_make_no_code_server_call() ->
    %% BT-4056: module-less ancestors and the class itself must not be probed
    %% by `new`, dispatch lookup or has_method_local/2. `new` runs inside the
    %% class gen_server, so trace that process as well as the worker.
    Base = start_class('ZzMlBase', undefined, 'Object', [foo]),
    Sub = start_class('ZzMlSub', undefined, 'ZzMlBase'),
    try
        {Result, Calls} = calls_during([{code_server, call, 1}], [Base, Sub], fun() ->
            _ = gen_server:call(Sub, {new, []}),
            _ = gen_server:call(Base, {new, []}),
            %% `foo` is defined on Base, so the walk reaches
            %% dispatchable_module/1 (and its class-name thunk).
            _ = beamtalk_dispatch:lookup(
                foo, [], #{'$beamtalk_class' => 'ZzMlSub'}, #{}, 'ZzMlSub'
            ),
            _ = beamtalk_object_class:has_method_local(Base, foo),
            ok
        end),
        ?assertEqual(ok, Result),
        ?assertEqual([], [R || {code_server, call, [R]} <- Calls])
    after
        beamtalk_test_proc:stop(Sub),
        beamtalk_test_proc:stop(Base)
    end.

warm_value_send_does_no_string_work() ->
    %% BT-4064: after the first send, resolving a Value class's dispatch module
    %% is a cache hit — no CamelCase -> snake_case conversion
    %% (beamtalk_module_name, built on `string`) and no superclass walk.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    Duration = #{'$beamtalk_class' => 'Duration', millis => 5},
    try
        %% Warm both classes (the first send derives and caches).
        7 = beamtalk_primitive:send(Self, getX, []),
        5 = beamtalk_primitive:send(Duration, asMilliseconds, []),
        {Results, Calls} = calls_during(
            [
                {beamtalk_module_name, '_', '_'},
                {string, '_', '_'},
                {beamtalk_class_registry, inherits_from, 2},
                {code_server, call, 1}
            ],
            fun() ->
                [
                    beamtalk_primitive:send(Self, getX, []),
                    beamtalk_primitive:responds_to(Self, getX),
                    beamtalk_primitive:send(Duration, asMilliseconds, []),
                    beamtalk_primitive:responds_to(Duration, asMilliseconds)
                ]
            end
        ),
        ?assertEqual([7, true, 5, true], Results),
        ?assertEqual([], Calls)
    after
        beamtalk_test_proc:stop(Pid)
    end.

warm_class_of_object_builds_no_atoms() ->
    %% With the class object cached, a warm class_of_object_by_name/1
    %% is a counter read + ETS read + pid liveness check — no registry_name,
    %% class_object_tag, whereis or metadata read. The cold call is traced
    %% first as a positive control that the patterns actually catch those.
    Patterns = [
        {beamtalk_class_registry, '_', '_'},
        {beamtalk_class_metadata, '_', '_'},
        {beamtalk_object_class, module_name, 1},
        {erlang, list_to_atom, 1},
        {erlang, atom_to_list, 1},
        {erlang, binary_to_atom, 2},
        {erlang, atom_to_binary, 2}
    ],
    Pid = start_class(?CLASS, ?MOD),
    try
        {Cold, ColdCalls} = calls_during(Patterns, fun() ->
            beamtalk_primitive:class_of_object_by_name(?CLASS)
        end),
        ?assertMatch({beamtalk_object, _, _, Pid}, Cold),
        ?assertNotEqual([], [C || C = {beamtalk_class_registry, _, _} <- ColdCalls]),
        {Warm, WarmCalls} = calls_during(Patterns, fun() ->
            [beamtalk_primitive:class_of_object_by_name(?CLASS) || _ <- lists:seq(1, 10)]
        end),
        ?assertEqual([Cold || _ <- lists:seq(1, 10)], Warm),
        ?assertEqual([], WarmCalls)
    after
        beamtalk_test_proc:stop(Pid)
    end.

exception_bit_follows_superclass_change() ->
    %% The cached "is this an Exception subclass?" bit depends on the class's
    %% superclass row; re-parenting the class (merge_identity/5, the hot-reload
    %% write) must be seen by the next send.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        ?assert(beamtalk_primitive:responds_to(Self, getX)),
        ?assertNot(beamtalk_class_derived_cache:inherits_from(?CLASS, 'Exception')),
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD, [], 'Exception', false),
        ?assert(beamtalk_class_derived_cache:inherits_from(?CLASS, 'Exception')),
        %% Now dispatched by bt@stdlib@exception, which has no getX.
        ?assertNot(beamtalk_primitive:responds_to(Self, getX)),
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD, [], none, false),
        ?assertNot(beamtalk_class_derived_cache:inherits_from(?CLASS, 'Exception')),
        ?assert(beamtalk_primitive:responds_to(Self, getX))
    after
        beamtalk_test_proc:stop(Pid)
    end.

purged_module_is_rederived() ->
    %% Code loading is not a metadata write, so the cache re-checks that a
    %% cached module is still loaded. Purging the class's module (the class
    %% stays registered) must not leave sends calling into a stale answer, and
    %% reloading it must bring the warm path back.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        ?assertEqual(7, beamtalk_primitive:send(Self, getX, [])),
        code:purge(?MOD),
        true = code:delete(?MOD),
        code:purge(?MOD),
        ?assertNot(erlang:module_loaded(?MOD)),
        ?assertBtError(does_not_understand, _, getX, beamtalk_primitive:send(Self, getX, [])),
        ok = load_accessor_module(?MOD, x),
        ?assertEqual(7, beamtalk_primitive:send(Self, getX, [])),
        {Results, Calls} = code_server_calls_during(fun() ->
            [beamtalk_primitive:send(Self, getX, []), beamtalk_primitive:send(Self, getX, [])]
        end),
        ?assertEqual([7, 7], Results),
        ?assertEqual([], Calls)
    after
        beamtalk_test_proc:stop(Pid),
        _ = load_accessor_module(?MOD, x)
    end.

inherited_object_send_makes_no_xref_call() ->
    %% `hasField:` is not in the class module, so the send falls through to
    %% runtime_instance_method/2, which resolves ?CLASS -> Object.
    Pid = start_class(?CLASS, ?MOD, 'Object'),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        true = beamtalk_primitive:send(Self, 'hasField:', [x]),
        {Results, Calls} = calls_during(
            [
                {beamtalk_xref, '_', '_'},
                {ets, tab2list, 1},
                {beamtalk_object_class, compiled_method_map, 7},
                {beamtalk_object_class, method, 2},
                {gen_server, call, '_'}
            ],
            fun() ->
                [
                    beamtalk_primitive:send(Self, 'hasField:', [x]),
                    beamtalk_primitive:send(Self, 'hasField:', [nope]),
                    beamtalk_primitive:responds_to(Self, 'hasField:')
                ]
            end
        ),
        ?assertEqual([true, false, true], Results),
        ?assertEqual([], Calls)
    after
        beamtalk_test_proc:stop(Pid)
    end.

runtime_method_changes_are_seen_by_next_send() ->
    %% The dispatch-time lookup is memoised; put_method (publish_reflection ->
    %% put_method_records/3) must invalidate it.
    Pid = start_class(?MODULE_LESS, undefined, 'Object'),
    Self = #{'$beamtalk_class' => ?MODULE_LESS},
    try
        ?assertBtError(does_not_understand, _, answer, beamtalk_primitive:send(Self, answer, [])),
        ?assertNot(beamtalk_primitive:responds_to(Self, answer)),
        ok = beamtalk_object_class:put_method(Pid, answer, fun(_Self) -> 42 end),
        ?assertEqual(42, beamtalk_primitive:send(Self, answer, [])),
        ?assert(beamtalk_primitive:responds_to(Self, answer)),
        ok = beamtalk_object_class:put_method(Pid, answer, fun(_Self) -> 43 end),
        ?assertEqual(43, beamtalk_primitive:send(Self, answer, []))
    after
        beamtalk_test_proc:stop(Pid)
    end,
    %% Re-parenting (merge_identity/5, the hot-reload write) is seen too.
    Pid2 = start_class(?MODULE_LESS, undefined, none),
    try
        ?assertEqual(
            not_found, beamtalk_method_resolver:instance_method_info(?MODULE_LESS, 'hasField:')
        ),
        ok = beamtalk_class_metadata:merge_identity(
            ?MODULE_LESS, ?MODULE_LESS, [], 'Object', false
        ),
        ?assertMatch(
            {ok, _}, beamtalk_method_resolver:instance_method_info(?MODULE_LESS, 'hasField:')
        ),
        ok = beamtalk_class_metadata:merge_identity(?MODULE_LESS, ?MODULE_LESS, [], none, false),
        ?assertEqual(
            not_found, beamtalk_method_resolver:instance_method_info(?MODULE_LESS, 'hasField:')
        )
    after
        beamtalk_test_proc:stop(Pid2)
    end,
    %% Removing the class drops its cached resolution too.
    ?assertEqual(
        unpublished, beamtalk_method_resolver:instance_method_info(?MODULE_LESS, answer)
    ).

perform_finds_runtime_fun_methods() ->
    %% BT-4624: the reflective path (beamtalk_dispatch:lookup/5) must resolve
    %% fun-backed methods of module-less classes, including inherited ones,
    %% exactly as the direct send does.
    Parent = start_class(?MODULE_LESS, undefined, 'Object'),
    Child = start_class('PkgHotModuleLessChild', undefined, ?MODULE_LESS),
    Self = #{'$beamtalk_class' => 'PkgHotModuleLessChild'},
    try
        ok = beamtalk_object_class:put_method(Parent, answer, fun(_S) -> 42 end),
        ok = beamtalk_object_class:put_method(Parent, 'plus:', fun(_S, N) -> N + 1 end),
        ?assertEqual(42, beamtalk_primitive:send(Self, answer, [])),
        ?assertEqual(42, beamtalk_primitive:send(Self, 'perform:', [answer])),
        ?assertEqual(
            8, beamtalk_primitive:send(Self, 'perform:withArguments:', ['plus:', [7]])
        ),
        ?assertEqual(
            8,
            beamtalk_primitive:send(
                Self, 'perform:withArguments:timeout:', ['plus:', [7], 1000]
            )
        ),
        ?assertEqual(
            {reply, 42, #{}},
            beamtalk_dispatch:lookup(answer, [], Self, #{}, 'PkgHotModuleLessChild')
        ),
        ?assertMatch(
            {error, #beamtalk_error{kind = does_not_understand}},
            beamtalk_dispatch:lookup(nope, [], Self, #{}, 'PkgHotModuleLessChild')
        ),
        %% A fun overriding a selector a compiled ancestor (Object) defines must
        %% win under perform: exactly as under a direct send.
        ok = beamtalk_object_class:put_method(Parent, 'printString', fun(_S) -> <<"foo">> end),
        ?assertEqual(<<"foo">>, beamtalk_primitive:send(Self, 'printString', [])),
        ?assertEqual(
            <<"foo">>, beamtalk_primitive:send(Self, 'perform:', ['printString'])
        )
    after
        beamtalk_test_proc:stop(Child),
        beamtalk_test_proc:stop(Parent)
    end.

perform_relays_nlr_and_script_exit_from_runtime_fun() ->
    %% BT-4642: a `^` NLR throw (ADR 0041) or script exit (ADR 0099 §3) raised in
    %% a fun-backed method (or a block passed to it) is control flow: perform:
    %% must let it propagate exactly as a direct send does, not wrap it.
    Pid = start_class(?MODULE_LESS, undefined, 'Object'),
    Self = #{'$beamtalk_class' => ?MODULE_LESS},
    Nlr = ?NLR(make_ref(), 7, #{}),
    try
        ok = beamtalk_object_class:put_method(Pid, 'do:', fun(_S, Blk) -> Blk(1) end),
        Throwing = fun(_) -> erlang:throw(Nlr) end,
        ?assertThrow(Nlr, beamtalk_primitive:send(Self, 'do:', [Throwing])),
        ?assertThrow(
            Nlr, beamtalk_primitive:send(Self, 'perform:withArguments:', ['do:', [Throwing]])
        ),
        Exit = {beamtalk_script_exit, 3},
        ExitBlk = fun(_) -> erlang:throw(Exit) end,
        ?assertThrow(Exit, beamtalk_primitive:send(Self, 'do:', [ExitBlk])),
        ?assertThrow(
            Exit, beamtalk_primitive:send(Self, 'perform:withArguments:', ['do:', [ExitBlk]])
        )
    after
        beamtalk_test_proc:stop(Pid)
    end.

method_origin_copies_no_generation_table() ->
    %% Reflection (`CompiledMethod origin`) still reads xref, but a one-class
    %% read snapshots only that class's generation (read_stable/2), not the
    %% whole xref_class_gen table twice.
    {Results, Calls} = calls_during(
        [{ets, tab2list, 1}],
        fun() ->
            [
                beamtalk_xref:method_origin('Object', false, 'hasField:'),
                beamtalk_xref:method_info('Object', false, 'hasField:')
            ]
        end
    ),
    ?assertMatch([nil, _], Results),
    ?assertEqual([], Calls).

memo_hit_reads_no_ets() ->
    %% BT-4623: the first dispatch in a process fetches from the derived cache
    %% and memoises; later ones in the same process read no ETS entry for it.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        {Results, Calls} = calls_during([{ets, lookup, 2}], fun() ->
            [beamtalk_primitive:tagged_map_dispatch(Self) || _ <- lists:seq(1, 5)]
        end),
        ?assertEqual([{value, ?CLASS, ?MOD} || _ <- lists:seq(1, 5)], Results),
        ?assertEqual(
            1,
            length([
                K
             || {ets, lookup, [beamtalk_class_derived_cache, {tagged_class_dispatch, _} = K]} <-
                    Calls
            ])
        ),
        _ = beamtalk_primitive:tagged_map_dispatch(Self),
        ?assertEqual(
            {beamtalk_class_derived_cache:generation(), {value, ?MOD}}, memo(?CLASS)
        )
    after
        beamtalk_test_proc:stop(Pid)
    end.

memo_follows_reregistration() ->
    %% A module move in the same process (merge_identity/5) bumps the
    %% generation, so the memo is a miss and the class is re-resolved.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        ?assertEqual({value, ?CLASS, ?MOD}, beamtalk_primitive:tagged_map_dispatch(Self)),
        ?assertMatch({_, {value, ?MOD}}, memo(?CLASS)),
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD2, [], none, false),
        ?assertEqual({value, ?CLASS, ?MOD2}, beamtalk_primitive:tagged_map_dispatch(Self)),
        ?assertMatch({_, {value, ?MOD2}}, memo(?CLASS))
    after
        beamtalk_test_proc:stop(Pid)
    end.

memo_follows_ancestor_reparent() ->
    %% Reparenting an ancestor into Exception makes an already-memoised
    %% subclass an Exception too; it must resolve to `exception`.
    Parent = start_class('ZzMemoParent', undefined, 'Object'),
    Child = start_class(?CLASS, ?MOD, 'ZzMemoParent'),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        ?assertEqual({value, ?CLASS, ?MOD}, beamtalk_primitive:tagged_map_dispatch(Self)),
        ?assertMatch({_, {value, ?MOD}}, memo(?CLASS)),
        ok = beamtalk_class_metadata:merge_identity(
            'ZzMemoParent', 'ZzMemoParent', [], 'Exception', false
        ),
        ?assertEqual(
            {module, 'bt@stdlib@exception'}, beamtalk_primitive:tagged_map_dispatch(Self)
        ),
        ?assertMatch({_, exception}, memo(?CLASS))
    after
        beamtalk_test_proc:stop(Child),
        beamtalk_test_proc:stop(Parent)
    end.

memo_of_purged_module_is_rederived() ->
    %% Purging is not a metadata write, so the generation stays put: the memo
    %% hit's own purge check must send it back to the derived cache.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    try
        ?assertEqual({value, ?CLASS, ?MOD}, beamtalk_primitive:tagged_map_dispatch(Self)),
        Gen = beamtalk_class_derived_cache:generation(),
        ?assertEqual({Gen, {value, ?MOD}}, memo(?CLASS)),
        code:purge(?MOD),
        true = code:delete(?MOD),
        code:purge(?MOD),
        ?assertEqual(Gen, beamtalk_class_derived_cache:generation()),
        ?assertBtError(does_not_understand, _, getX, beamtalk_primitive:send(Self, getX, [])),
        ok = load_accessor_module(?MOD, x),
        ?assertEqual(7, beamtalk_primitive:send(Self, getX, []))
    after
        beamtalk_test_proc:stop(Pid),
        _ = load_accessor_module(?MOD, x)
    end.

uncacheable_answer_is_not_memoised() ->
    %% An unregistered class's answer comes from the code path, which the
    %% cache does not track, so Compute says `nocache` and nothing is memoised
    %% (and no `nocache` marker is left behind).
    Class = 'ZzMemoUnregistered',
    ?assertEqual(not_found, beamtalk_class_metadata:lookup_module(Class)),
    ?assertMatch(
        {value, Class, _}, beamtalk_primitive:tagged_map_dispatch(#{'$beamtalk_class' => Class})
    ),
    ?assertEqual(undefined, memo(Class)).

no_generation_memoises_nothing() ->
    %% Before beamtalk_class_derived_cache:new/0 there is no generation: every
    %% call computes, the answer is still right, and nothing is memoised.
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7},
    GenKey = {beamtalk_class_derived_cache, generation},
    Ref = persistent_term:get(GenKey),
    try
        erase({beamtalk_class_derived_cache, memo}),
        persistent_term:erase(GenKey),
        ?assertEqual(undefined, beamtalk_class_derived_cache:generation()),
        ?assertEqual({value, ?CLASS, ?MOD}, beamtalk_primitive:tagged_map_dispatch(Self)),
        ?assertEqual({value, ?CLASS, ?MOD}, beamtalk_primitive:tagged_map_dispatch(Self)),
        ?assertEqual(undefined, memo(?CLASS))
    after
        persistent_term:put(GenKey, Ref),
        beamtalk_class_derived_cache:invalidate(),
        beamtalk_test_proc:stop(Pid)
    end.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

start_class(Name, Module) ->
    start_class(Name, Module, none).

start_class(Name, Module, Superclass) ->
    start_class(Name, Module, Superclass, []).

start_class(Name, Module, Superclass, Selectors) ->
    Base = #{
        superclass => Superclass,
        class_methods => #{},
        class_state => #{},
        instance_methods => maps:from_list([{S, #{}} || S <- Selectors])
    },
    ClassInfo =
        case Module of
            undefined -> Base;
            _ -> Base#{module => Module}
        end,
    beamtalk_test_class:start(Name, ClassInfo).

%% The calling process's tagged_class_dispatch/1 memo for Class (BT-4623).
memo(Class) ->
    %% The derived cache's per-process memo (BT-4652): {Gen, #{Key => Value}}.
    case get({beamtalk_class_derived_cache, memo}) of
        {Gen, Map} ->
            case maps:find({tagged_class_dispatch, Class}, Map) of
                {ok, V} -> {Gen, V};
                error -> undefined
            end;
        undefined ->
            undefined
    end.

-doc """
Load a module exporting `getX/1`, which returns `Field` from the receiver map —
the shape of a compiled Value class's getter (`Module:Selector(Self)`).
""".
load_accessor_module(Module, Field) ->
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 1, export, [{getX, 1}]},
        {function, 1, getX, 1, [
            {clause, 1, [{var, 1, 'Self'}], [], [
                {call, 1, {remote, 1, {atom, 1, maps}, {atom, 1, get}}, [
                    {atom, 1, Field}, {var, 1, 'Self'}
                ]}
            ]}
        ]}
    ],
    {ok, Module, Binary} = compile:forms(Forms),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary),
    ok.

-doc """
Run `Fun` in a fresh process and return `{Result, CodeServerRequests}`, where
`CodeServerRequests` lists every `code_server:call/1` request that process
made. The worker is traced rather than the test process because a process
cannot be its own tracer.
""".
code_server_calls_during(Fun) ->
    {Result, Calls} = calls_during([{code_server, call, 1}], Fun),
    {Result, [Request || {code_server, call, [Request]} <- Calls]}.

-doc """
Run `Fun` in a fresh process and return `{Result, Calls}`, where `Calls` lists
every `{M, F, Args}` call that process made to a function matching one of
`Patterns` (`{M, F, Arity}`, `'_'` wildcards allowed, traced as local calls).
""".
calls_during(Patterns, Fun) ->
    calls_during(Patterns, [], Fun).

-doc """
As `calls_during/2`, additionally tracing `ExtraPids` (e.g. class gen_servers
that run part of `Fun`'s work); their calls are included in `Calls`.
""".
calls_during(Patterns, ExtraPids, Fun) ->
    Parent = self(),
    Worker = spawn(fun() ->
        receive
            go -> ok
        end,
        Parent !
            {
                result,
                self(),
                try
                    Fun()
                catch
                    throw:BtThrown -> BtThrown;
                    error:BtReason:BtStack -> {'EXIT', {BtReason, BtStack}};
                    exit:BtExitReason -> {'EXIT', BtExitReason}
                end
            }
    end),
    lists:foreach(fun(P) -> erlang:trace_pattern(P, true, [local]) end, Patterns),
    %% Always remove the local trace patterns, even on a timeout, so they never
    %% leak into (and slows down) the rest of the EUnit run, and never leave
    %% the (unlinked, unmonitored) worker behind: if erlang:trace/3 fails it
    %% never receives `go`, and on worker_timeout it is still running. On the
    %% success path it has already exited, so the kill is a no-op.
    Result =
        try
            lists:foreach(fun(P) -> 1 = erlang:trace(P, true, [call]) end, [Worker | ExtraPids]),
            Worker ! go,
            receive
                {result, Worker, R} -> R
            after 5000 -> error(worker_timeout)
            end
        after
            lists:foreach(fun(P) -> erlang:trace_pattern(P, false, [local]) end, Patterns),
            exit(Worker, kill)
        end,
    %% Make sure every trace message the worker generated has arrived.
    Pids = [Worker | ExtraPids],
    lists:foreach(
        fun(P) ->
            Ref = erlang:trace_delivered(P),
            receive
                {trace_delivered, P, Ref} -> ok
            after 5000 -> error(trace_delivery_timeout)
            end
        end,
        Pids
    ),
    {Result, drain_calls(Pids, [])}.

drain_calls(Pids, Acc) ->
    receive
        {trace, P, call, MFA} when is_pid(P) ->
            case lists:member(P, Pids) of
                true -> drain_calls(Pids, [MFA | Acc]);
                false -> drain_calls(Pids, Acc)
            end
    after 0 -> lists:reverse(Acc)
    end.
