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
        {"tagged_class_dispatch per-process memo (BT-4623)", fun tagged_class_dispatch_memo/0},
        {"inherited Object send makes no xref or CompiledMethod work (BT-4071)",
            fun inherited_object_send_makes_no_xref_call/0},
        {"fun-backed methods added or replaced at runtime are seen by the next send (BT-4071)",
            fun runtime_method_changes_are_seen_by_next_send/0},
        {"BT-4624: perform: finds fun-backed methods of a module-less class",
            fun perform_finds_runtime_fun_methods/0},
        {"method_origin/3 copies no generation table (BT-4071)",
            fun method_origin_copies_no_generation_table/0}
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
        stop_class(Pid)
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
        stop_class(Pid)
    end,
    ?assertEqual(not_found, beamtalk_class_metadata:lookup_module(?CLASS)),
    ?assertEqual(
        beamtalk_module_name:to_module_atom(?CLASS),
        beamtalk_primitive:class_name_to_module(?CLASS)
    ),
    ?assertError(
        #{error := #beamtalk_error{kind = does_not_understand, selector = getX}},
        beamtalk_primitive:send(Self, getX, [])
    ).

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
        stop_class(Pid)
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
        stop_class(Sub),
        stop_class(Base)
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
        stop_class(Pid)
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
        stop_class(Pid)
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
        stop_class(Pid)
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
        ?assertError(
            #{error := #beamtalk_error{kind = does_not_understand, selector = getX}},
            beamtalk_primitive:send(Self, getX, [])
        ),
        ok = load_accessor_module(?MOD, x),
        ?assertEqual(7, beamtalk_primitive:send(Self, getX, [])),
        {Results, Calls} = code_server_calls_during(fun() ->
            [beamtalk_primitive:send(Self, getX, []), beamtalk_primitive:send(Self, getX, [])]
        end),
        ?assertEqual([7, 7], Results),
        ?assertEqual([], Calls)
    after
        stop_class(Pid),
        _ = load_accessor_module(?MOD, x)
    end.

tagged_class_dispatch_memo() ->
    %% BT-4623: a warm tagged_class_dispatch/1 is answered from a per-process
    %% memo tagged with the derived-cache generation.
    Memo = {beamtalk_primitive, tagged_class_dispatch, ?CLASS},
    Pid = start_class(?CLASS, ?MOD),
    Self = #{'$beamtalk_class' => ?CLASS, x => 7, y => 2},
    try
        erase(Memo),
        ?assertEqual(7, beamtalk_primitive:send(Self, getX, [])),
        Gen = beamtalk_class_derived_cache:generation(),
        ?assertEqual({Gen, {value, ?MOD}}, get(Memo)),
        %% A hit is answered by the memo: a poisoned (but current, loaded)
        %% memo is returned without consulting the cache.
        put(Memo, {Gen, {stdlib, 'bt@stdlib@dictionary'}}),
        ?assertEqual(
            {stdlib, 'bt@stdlib@dictionary'}, beamtalk_primitive:tagged_class_dispatch(?CLASS)
        ),
        erase(Memo),
        ?assertEqual({value, ?MOD}, beamtalk_primitive:tagged_class_dispatch(?CLASS)),
        %% Moving the class to another module is re-resolved in this process.
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD2, [], none, false),
        ?assertEqual(?MOD2, beamtalk_primitive:class_name_to_module(?CLASS)),
        ?assertEqual({value, ?MOD2}, beamtalk_primitive:tagged_class_dispatch(?CLASS)),
        ?assertEqual(
            {beamtalk_class_derived_cache:generation(), {value, ?MOD2}}, get(Memo)
        ),
        %% Reparenting into Exception re-resolves an already-memoised class.
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD2, [], 'Exception', false),
        ?assertEqual(exception, beamtalk_primitive:tagged_class_dispatch(?CLASS)),
        ?assertMatch({_, exception}, get(Memo)),
        ok = beamtalk_class_metadata:merge_identity(?CLASS, ?MOD2, [], none, false),
        ?assertEqual(2, beamtalk_primitive:send(Self, getX, [])),
        %% A purged module whose class is still registered is re-derived.
        code:purge(?MOD2),
        true = code:delete(?MOD2),
        code:purge(?MOD2),
        ?assertError(
            #{error := #beamtalk_error{kind = does_not_understand, selector = getX}},
            beamtalk_primitive:send(Self, getX, [])
        )
    after
        stop_class(Pid),
        _ = load_accessor_module(?MOD2, y),
        erase(Memo)
    end,
    %% An unregistered class is not memoised.
    Unreg = 'PkgHotUnregistered',
    UMemo = {beamtalk_primitive, tagged_class_dispatch, Unreg},
    erase(UMemo),
    _ = (catch beamtalk_primitive:send(#{'$beamtalk_class' => Unreg}, foo, [])),
    ?assertEqual(undefined, get(UMemo)).

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
        stop_class(Pid)
    end.

runtime_method_changes_are_seen_by_next_send() ->
    %% The dispatch-time lookup is memoised; put_method (publish_reflection ->
    %% put_method_records/3) must invalidate it.
    Pid = start_class(?MODULE_LESS, undefined, 'Object'),
    Self = #{'$beamtalk_class' => ?MODULE_LESS},
    try
        ?assertError(
            #{error := #beamtalk_error{kind = does_not_understand, selector = answer}},
            beamtalk_primitive:send(Self, answer, [])
        ),
        ?assertNot(beamtalk_primitive:responds_to(Self, answer)),
        ok = beamtalk_object_class:put_method(Pid, answer, fun(_Self) -> 42 end),
        ?assertEqual(42, beamtalk_primitive:send(Self, answer, [])),
        ?assert(beamtalk_primitive:responds_to(Self, answer)),
        ok = beamtalk_object_class:put_method(Pid, answer, fun(_Self) -> 43 end),
        ?assertEqual(43, beamtalk_primitive:send(Self, answer, []))
    after
        stop_class(Pid)
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
        stop_class(Pid2)
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
        stop_class(Child),
        stop_class(Parent)
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
    {ok, Pid} = beamtalk_object_class:start_link(Name, ClassInfo),
    unlink(Pid),
    Pid.

stop_class(Pid) ->
    catch gen_server:stop(Pid, normal, 5000),
    ok.

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
        Parent ! {result, self(), catch Fun()}
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
