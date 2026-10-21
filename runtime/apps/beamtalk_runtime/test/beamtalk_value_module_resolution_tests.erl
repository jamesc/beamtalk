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
        {"warm Value sends do no module-name string work (BT-4064)",
            fun warm_value_send_does_no_string_work/0},
        {"exception bit follows a superclass change (BT-4064)",
            fun exception_bit_follows_superclass_change/0},
        {"a cached module that is purged and reloaded is re-derived (BT-4064)",
            fun purged_module_is_rederived/0},
        {"inherited Object send makes no xref or CompiledMethod work (BT-4071)",
            fun inherited_object_send_makes_no_xref_call/0},
        {"fun-backed methods added or replaced at runtime are seen by the next send (BT-4071)",
            fun runtime_method_changes_are_seen_by_next_send/0},
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
    %% Removing the class drops its cached resolution too.
    ?assertEqual(
        unpublished, beamtalk_method_resolver:instance_method_info(?MODULE_LESS, answer)
    ).

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
    Base = #{superclass => Superclass, class_methods => #{}, class_state => #{}},
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
            1 = erlang:trace(Worker, true, [call]),
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
    Ref = erlang:trace_delivered(Worker),
    receive
        {trace_delivered, Worker, Ref} -> ok
    after 5000 -> error(trace_delivery_timeout)
    end,
    {Result, drain_calls(Worker, [])}.

drain_calls(Worker, Acc) ->
    receive
        {trace, Worker, call, MFA} ->
            drain_calls(Worker, [MFA | Acc])
    after 0 -> lists:reverse(Acc)
    end.
