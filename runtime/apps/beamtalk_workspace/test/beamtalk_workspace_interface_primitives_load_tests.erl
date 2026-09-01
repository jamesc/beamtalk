%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_interface_primitives_load_tests).

-moduledoc """
EUnit coverage (BT-3336) for the `beamtalk_workspace_interface_primitives.erl`
branches that need a live compiler + a registered class to exercise — the
`load:`/supervisor/singleton paths BT-2393 originally flagged and BT-3334's
epic picks back up:

  - `load/1` / `handle_load/1`: the SUCCESS path (a real `.bt` file compiled
    and registered, its source recorded via `workspace_meta:set_class_source/2`,
    `loaded_class_objects/1` resolving the live class pid), the semantic
    compile-error path (`ensure_structured_error/1`), and the native `.erl`
    compilation-failure path (`maybe_recompile_native_deps/2` finding a
    referenced-but-broken native module before the `.bt` file is even
    compiled). Only the type-error and file-not-found arms had EUnit coverage
    before this module — see `beamtalk_workspace_interface_primitives_tests.erl`.
  - `moveClass/2`: the success path (dispatch routing, source-path update,
    same class pid/name preserved) — previously untested at any layer
    (`dispatch('moveClass:to:', ...)`, line 148, had zero EUnit coverage).
  - `newClass/2`: the success path (a brand-new class installed and returned)
    complementing the existing arg-type-error coverage.
  - `startSupervisor/1` / `stopSupervisor/1`: the full attach/detach lifecycle
    against a real `Supervisor subclass:` — idempotent re-attach
    (`{error, {already_started, _}}`), re-attach after a manual stop
    (`{error, already_present}`), and the "not attached to the workspace"
    refusal — complementing the existing type-error-only coverage.
  - `dependencies/0`: the populated-map branch with a real dependency name AND
    the `catch error:_ -> false` branch for a dependency `beamtalk_package:named/1`
    cannot resolve — both needed `beamtalk_package`/`beamtalk_workspace_meta`
    faulted or fed real data, which the existing "empty package" tests don't
    cover.
  - `resolve_name/2` / `resolve_class_reference/2` Tier 3 (singleton) and
    Tier 4 (class registry) — the existing suite only reaches Tier 1 (locals),
    Tier 2 (`bind:as:`), and Tier 5 (undefined); a real class + the
    `WorkspaceInterface` singleton itself close the remaining tiers.
  - `resolve_singleton_instance/1` — the REPL codegen binding-aware
    class-send fallback surface, untested until now.

These boot the full stack (compiler port + runtime + workspace_meta +
changelog) against an isolated, in-project temp tree, mirroring
`beamtalk_workspace_revert_tests.erl`'s scaffolding. Each test uses a UNIQUE
workspace_id + temp project dir and cleans up in an `after`/teardown block.

BT-2962 spike (OTP 29 native records): on this branch, the `meck:new(beamtalk_package, ...)`
calls in the `dependencies/0` tests below crash — `meck` rebuilds a mock
module's attributes from `Mod:module_info(attributes)`, which list-wraps
`-import_record`'s value even for a single occurrence, and
`erl_lint:import_native_record/3` has no clause for that shape. See the
BT-2962 Linear issue for the full writeup.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% supervisor behaviour callback used by with_bare_workspace_sup/1 (a minimal
%% childless workspace_sup stand-in, mirroring
%% beamtalk_workspace_interface_primitives_tests:start_bare_workspace_sup/0).
-behaviour(supervisor).
-export([init/1]).

%%====================================================================
%% Suite-level setup: the heavy, node-global apps (compiler port + runtime).
%%====================================================================

primitives_load_test_() ->
    {setup, fun suite_setup/0, fun suite_teardown/1,
        {foreach, fun case_setup/0, fun case_teardown/1, [
            fun load_success_returns_class_object/1,
            fun load_semantic_compile_error_is_structured/1,
            fun load_native_compile_failure_is_structured/1,
            fun move_class_success_updates_source_path/1,
            fun move_class_via_dispatch_success/1,
            fun new_class_success_installs_and_returns_class/1,
            fun dependencies_with_real_dependency_returns_populated_map/1,
            fun dependencies_skips_a_dependency_that_fails_to_resolve/1,
            fun start_supervisor_attaches_and_is_idempotent/1,
            fun start_supervisor_restarts_after_child_spec_deleted/1,
            fun stop_supervisor_detaches_workspace_child/1,
            fun stop_supervisor_not_attached_raises_runtime_error/1,
            fun resolve_name_singleton_tier_hit/1,
            fun resolve_name_class_registry_tier_hit/1,
            fun resolve_class_reference_singleton_tier_hit/1,
            fun resolve_class_reference_class_registry_tier_hit/1,
            fun resolve_singleton_instance_hit_and_miss/1
        ]}}.

suite_setup() ->
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Let the runtime register bootstrap classes before compiling user code.
    timer:sleep(300),
    ok.

suite_teardown(_) ->
    ok.

%% Per-case: an isolated workspace (unique id + temp HOME/project) plus the
%% changelog and meta gen_servers, with `project_path` pointing at the temp
%% tree so files written there classify as in-project.
case_setup() ->
    Unique = beamtalk_test_unique:id(),
    WorkspaceId = list_to_binary("wi-load-e2e-" ++ Unique),
    Tmp = filename:join(temp_dir(), "bt-wi-load-e2e-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    stop_existing(beamtalk_workspace_changelog),
    stop_existing(beamtalk_workspace_meta),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(Tmp),
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }),
    #{
        clog_pid => ClogPid,
        meta_pid => MetaPid,
        workspace_id => WorkspaceId,
        tmp => Tmp,
        unique => Unique,
        old_home => OldHome
    }.

case_teardown(#{clog_pid := ClogPid, meta_pid := MetaPid, tmp := Tmp, old_home := OldHome}) ->
    stop_proc(MetaPid),
    stop_proc(ClogPid),
    restore_home(OldHome),
    _ = file:del_dir_r(Tmp),
    ok.

%%====================================================================
%% load/1 — handle_load/1 (BT-2091) success + error-shape paths
%%====================================================================

%% A real `.bt` file compiles, registers, records its source (so a later
%% `Class >> selector => body` patch resolves its span), and `load/1` returns
%% the loaded class object.
load_success_returns_class_object(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiLoadOk" ++ U),
    Path = filename:join(Tmp, binary_to_list(ClassName) ++ ".bt"),
    ok = file:write_file(
        Path, iolist_to_binary([<<"Object subclass: ">>, ClassName, <<"\n  value => 1\n">>])
    ),
    Result = beamtalk_workspace_interface_primitives:load(list_to_binary(Path)),
    ClassAtom = binary_to_atom(ClassName, utf8),
    [
        ?_assertMatch([#beamtalk_object{}], Result),
        ?_assertNotEqual(
            undefined, beamtalk_workspace_meta:get_class_source(ClassName)
        ),
        ?_assert(is_pid(beamtalk_runtime_api:whereis_class(ClassAtom)))
    ].

%% A `.bt` file with a genuine syntax error surfaces as a structured error
%% (via `ensure_structured_error/1`), not a raw compiler tuple or a crash.
load_semantic_compile_error_is_structured(#{tmp := Tmp, unique := U}) ->
    Path = filename:join(Tmp, "WiLoadBroken" ++ U ++ ".bt"),
    ok = file:write_file(Path, <<"invalid @@@ syntax">>),
    [
        ?_assertException(
            error,
            #{error := #beamtalk_error{}},
            beamtalk_workspace_interface_primitives:load(list_to_binary(Path))
        )
    ].

%% A `.bt` file referencing a native module whose `.erl` fails to compile is
%% rejected with a `native_compile_failed` structured error, before the `.bt`
%% file itself is even handed to the compiler (BT-2091's native pre-step).
load_native_compile_failure_is_structured(#{tmp := Tmp, unique := U}) ->
    ok = file:write_file(
        filename:join(Tmp, "beamtalk.toml"), <<"[package]\nname = \"wiloadnative\"\n">>
    ),
    NativeDir = filename:join(Tmp, "native"),
    ok = file:make_dir(NativeDir),
    ModName = "wi_load_native_broken_" ++ U,
    ok = file:write_file(
        filename:join(NativeDir, ModName ++ ".erl"),
        iolist_to_binary([<<"-module(">>, ModName, <<").\ngo( -> broken.\n">>])
    ),
    ClassName = "WiLoadNativeUser" ++ U,
    Path = filename:join(Tmp, ClassName ++ ".bt"),
    ok = file:write_file(
        Path,
        iolist_to_binary([
            <<"Object subclass: ">>,
            ClassName,
            <<"\n  go => (Erlang ">>,
            ModName,
            <<") go\n">>
        ])
    ),
    try
        beamtalk_workspace_interface_primitives:load(list_to_binary(Path)),
        [?_assert(false)]
    catch
        error:#{error := #beamtalk_error{kind = Kind, selector = Selector}} ->
            [
                ?_assertEqual(native_compile_failed, Kind),
                ?_assertEqual('load:', Selector)
            ]
    end.

%%====================================================================
%% moveClass/2 (ADR 0114 Phase 2, BT-3272) — success path
%%====================================================================

move_class_success_updates_source_path(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiMoveOk" ++ U),
    {ClassObj, OldPath} = define_project_class(Tmp, ClassName),
    NewPath = list_to_binary(filename:join(Tmp, binary_to_list(ClassName) ++ "_moved.bt")),
    EntriesBefore = length(beamtalk_workspace_changelog:entries()),
    Result = beamtalk_workspace_interface_primitives:moveClass(ClassObj, NewPath),
    EntriesAfter = beamtalk_workspace_changelog:entries(),
    ClassAtom = binary_to_atom(ClassName, utf8),
    %% moveClass:to: is a pure filesystem-organization ChangeEntry, like
    %% newClass:at: — no disk write until `Workspace flush` (ADR 0114 Phase 2
    %% mirrors ADR 0082 Phase 1's deferred-write contract). The class's own
    %% pid never moves.
    [MoveEntry] = [
        E
     || E <- EntriesAfter,
        beamtalk_workspace_changelog:entry_kind(E) =:= 'rename-class',
        beamtalk_workspace_changelog:entry_class(E) =:= ClassName
    ],
    [
        %% moveClass:to: returns the same class object unchanged.
        ?_assertEqual(ClassObj, Result),
        %% The class is still live under the same pid/name.
        ?_assertEqual(
            beamtalk_runtime_api:whereis_class(ClassAtom),
            erlang:element(4, ClassObj)
        ),
        ?_assertEqual(EntriesBefore + 1, length(EntriesAfter)),
        ?_assert(MoveEntry =/= undefined),
        %% Deferred write (ADR 0082 Phase 2 / ADR 0114 Phase 2): the old file
        %% still holds the source until an explicit `Workspace flush`.
        ?_assert(filelib:is_regular(OldPath)),
        ?_assertEqual(false, filelib:is_regular(binary_to_list(NewPath)))
    ].

%% dispatch('moveClass:to:', ...) routes to moveClass/2 — previously
%% completely uncovered (line 148 of the source).
move_class_via_dispatch_success(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiMoveDispatch" ++ U),
    {ClassObj, _OldPath} = define_project_class(Tmp, ClassName),
    NewPath = list_to_binary(filename:join(Tmp, binary_to_list(ClassName) ++ "_moved2.bt")),
    Self = fake_self(self()),
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'moveClass:to:', [ClassObj, NewPath], Self
    ),
    [?_assertEqual(ClassObj, Result)].

%%====================================================================
%% newClass/2 (ADR 0082 Phase 1, BT-2285) — success path
%%====================================================================

new_class_success_installs_and_returns_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiNewOk" ++ U),
    Path = list_to_binary(filename:join(Tmp, binary_to_list(ClassName) ++ ".bt")),
    Source = iolist_to_binary([<<"Object subclass: ">>, ClassName, <<"\n  value => 7\n">>]),
    Result = beamtalk_workspace_interface_primitives:newClass(Source, Path),
    ClassAtom = binary_to_atom(ClassName, utf8),
    [
        ?_assertMatch([#beamtalk_object{}], Result),
        ?_assert(is_pid(beamtalk_runtime_api:whereis_class(ClassAtom))),
        %% newClass:at: installs in memory only — the disk write happens later,
        %% at `Workspace flush` (ADR 0082 Phase 2).
        ?_assertEqual(false, filelib:is_regular(binary_to_list(Path)))
    ].

%%====================================================================
%% dependencies/0 (ADR 0070 Phase 5) — populated-map + resolve-failure branch
%%====================================================================

%% get_package_name/0 is read from workspace_meta's init-time state (cached,
%% not re-read per call), so beamtalk.toml must exist BEFORE workspace_meta
%% starts — `case_setup/0` already started one against `Tmp` with no
%% beamtalk.toml present yet, so this restarts it against the same
%% WorkspaceId/Tmp after writing the file (mirroring
%% `beamtalk_workspace_interface_primitives_tests:dependencies_with_package_name_returns_map_test/0`'s
%% write-before-start ordering).
dependencies_with_real_dependency_returns_populated_map(#{
    tmp := Tmp, workspace_id := WorkspaceId
}) ->
    ok = file:write_file(
        filename:join(Tmp, "beamtalk.toml"), <<"[package]\nname = \"wideps_pkg\"\n">>
    ),
    meck:new(beamtalk_package, [passthrough]),
    meck:expect(beamtalk_package, dependencies, fun(<<"wideps_pkg">>) -> [<<"utils">>] end),
    meck:expect(beamtalk_package, named, fun(<<"utils">>) -> #{name => <<"utils">>} end),
    try
        with_reloaded_meta(WorkspaceId, Tmp, fun() ->
            Result = beamtalk_workspace_interface_primitives:dependencies(),
            [?_assertEqual(#{<<"utils">> => #{name => <<"utils">>}}, Result)]
        end)
    after
        meck:unload(beamtalk_package)
    end.

%% A dependency name the package registry cannot resolve (`beamtalk_package:named/1`
%% raises) is silently skipped rather than crashing `dependencies/0` — the
%% `catch error:_ -> false` arm of the filtermap.
dependencies_skips_a_dependency_that_fails_to_resolve(#{tmp := Tmp, workspace_id := WorkspaceId}) ->
    ok = file:write_file(
        filename:join(Tmp, "beamtalk.toml"), <<"[package]\nname = \"wideps_pkg2\"\n">>
    ),
    meck:new(beamtalk_package, [passthrough]),
    meck:expect(beamtalk_package, dependencies, fun(<<"wideps_pkg2">>) ->
        [<<"missing_dep">>]
    end),
    meck:expect(beamtalk_package, named, fun(<<"missing_dep">>) -> error(not_found) end),
    try
        with_reloaded_meta(WorkspaceId, Tmp, fun() ->
            Result = beamtalk_workspace_interface_primitives:dependencies(),
            [?_assertEqual(#{}, Result)]
        end)
    after
        meck:unload(beamtalk_package)
    end.

%% Restart workspace_meta against the same WorkspaceId/Tmp so its init-time
%% `get_package_name/0` cache picks up a beamtalk.toml written after
%% `case_setup/0`'s original start. Runs `Fun/0` while the fresh meta is up,
%% then stops it; restoring the registered name is left to
%% `case_teardown/1` (which tolerates the already-dead original pid).
with_reloaded_meta(WorkspaceId, Tmp, Fun) ->
    stop_existing(beamtalk_workspace_meta),
    {ok, FreshMetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(Tmp),
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }),
    try
        Fun()
    after
        stop_proc(FreshMetaPid)
    end.

%%====================================================================
%% startSupervisor:/stopSupervisor: (BT-1341) — full lifecycle against a
%% real `Supervisor subclass:` and a bare, locally-registered
%% beamtalk_workspace_sup.
%%====================================================================

%% NOTE: `with_bare_workspace_sup/1`'s `after` clause stops the stand-in
%% supervisor as soon as its argument function returns — so every assertion
%% against that supervisor must run EAGERLY (?assert*, not the lazy ?_assert*
%% fixture forms) *inside* that function, wrapped in a single `?_test/1` so
%% EUnit defers setup + assertions + teardown together as one unit rather
%% than evaluating the (by-then-torn-down) lazy fixtures afterward.
start_supervisor_attaches_and_is_idempotent(#{tmp := Tmp, unique := U}) ->
    [
        ?_test(
            with_bare_workspace_sup(fun() ->
                {SupClassObj, SupClassAtom} = define_supervisor_class(Tmp, U),
                First = beamtalk_workspace_interface_primitives:startSupervisor(SupClassObj),
                Second = beamtalk_workspace_interface_primitives:startSupervisor(SupClassObj),
                ?assertMatch({beamtalk_supervisor, SupClassAtom, _Module, _Pid}, First),
                %% Idempotent: re-attaching an already-running supervisor
                %% returns the SAME handle rather than erroring or
                %% double-registering.
                ?assertEqual(First, Second),
                ?assertEqual(1, length(beamtalk_workspace_interface_primitives:supervisors()))
            end)
        )
    ].

%% After the child spec is deleted (stale, not running) a re-attach recreates
%% it (`{error, already_present}` branch of `do_start_supervisor/2`).
start_supervisor_restarts_after_child_spec_deleted(#{tmp := Tmp, unique := U}) ->
    [
        ?_test(
            with_bare_workspace_sup(fun() ->
                {SupClassObj, SupClassAtom} = define_supervisor_class(Tmp, U),
                {beamtalk_supervisor, SupClassAtom, _Mod, Pid1} =
                    beamtalk_workspace_interface_primitives:startSupervisor(SupClassObj),
                %% Kill the child directly (bypassing stopSupervisor:) so the
                %% spec is left registered but not running —
                %% "already_present" on restart.
                ChildId = {user_supervisor, SupClassAtom},
                ok = supervisor:terminate_child(beamtalk_workspace_sup, ChildId),
                Restarted = beamtalk_workspace_interface_primitives:startSupervisor(SupClassObj),
                ?assertMatch({beamtalk_supervisor, SupClassAtom, _, _}, Restarted),
                ?assertNotEqual(Pid1, erlang:element(4, Restarted))
            end)
        )
    ].

stop_supervisor_detaches_workspace_child(#{tmp := Tmp, unique := U}) ->
    [
        ?_test(
            with_bare_workspace_sup(fun() ->
                {SupClassObj, _SupClassAtom} = define_supervisor_class(Tmp, U),
                _ = beamtalk_workspace_interface_primitives:startSupervisor(SupClassObj),
                StopResult = beamtalk_workspace_interface_primitives:stopSupervisor(SupClassObj),
                ?assertEqual(nil, StopResult),
                ?assertEqual(0, length(beamtalk_workspace_interface_primitives:supervisors()))
            end)
        )
    ].

%% Stopping a Supervisor subclass that was never attached (and isn't the root
%% supervisor either) raises a directed runtime_error.
stop_supervisor_not_attached_raises_runtime_error(#{tmp := Tmp, unique := U}) ->
    [
        ?_test(
            with_bare_workspace_sup(fun() ->
                {SupClassObj, _SupClassAtom} = define_supervisor_class(Tmp, U),
                ?assertException(
                    error,
                    #{error := #beamtalk_error{kind = runtime_error, selector = 'stopSupervisor:'}},
                    beamtalk_workspace_interface_primitives:stopSupervisor(SupClassObj)
                )
            end)
        )
    ].

%%====================================================================
%% resolve_name/2, resolve_class_reference/2, resolve_singleton_instance/1 —
%% Tier 3 (singleton) and Tier 4 (class registry) hits (BT-2365, ADR 0081
%% Phase 1). The existing suite only reaches Tier 1/2/5.
%%====================================================================

%% Tier 3: `Workspace` is a configured singleton binding name; resolve_name/2
%% resolves it live via lookup_singleton/1 -> handle_session_bindings/1.
resolve_name_singleton_tier_hit(_Ctx) ->
    Result = beamtalk_workspace_interface_primitives:resolve_name(#{}, 'Workspace'),
    [?_assert(is_map(Result) orelse is_tuple(Result))].

%% Tier 4: a real registered class resolves to a `{beamtalk_object, '<Name>
%% class', Module, Pid}` tuple via lookup_class_object/1.
resolve_name_class_registry_tier_hit(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiResolveName" ++ U),
    {_ClassObj, _Path} = define_project_class(Tmp, ClassName),
    ClassAtom = binary_to_atom(ClassName, utf8),
    Result = beamtalk_workspace_interface_primitives:resolve_name(#{}, ClassAtom),
    [
        ?_assertMatch({beamtalk_object, _, _, _}, Result),
        ?_assertEqual(
            beamtalk_runtime_api:whereis_class(ClassAtom), erlang:element(4, Result)
        )
    ].

resolve_class_reference_singleton_tier_hit(_Ctx) ->
    Result = beamtalk_workspace_interface_primitives:resolve_class_reference(#{}, 'Workspace'),
    [?_assert(is_map(Result) orelse is_tuple(Result))].

resolve_class_reference_class_registry_tier_hit(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("WiResolveClsRef" ++ U),
    {_ClassObj, _Path} = define_project_class(Tmp, ClassName),
    ClassAtom = binary_to_atom(ClassName, utf8),
    Result = beamtalk_workspace_interface_primitives:resolve_class_reference(#{}, ClassAtom),
    [?_assertMatch({beamtalk_object, _, _, _}, Result)].

resolve_singleton_instance_hit_and_miss(_Ctx) ->
    Hit = beamtalk_workspace_interface_primitives:resolve_singleton_instance('Workspace'),
    Miss = beamtalk_workspace_interface_primitives:resolve_singleton_instance(
        'NotASingletonNameXyz'
    ),
    [
        ?_assertMatch({ok, _}, Hit),
        ?_assertEqual(error, Miss)
    ].

%%====================================================================
%% Helpers
%%====================================================================

%% Define a class backed by a real in-project `.bt` file (mirrors
%% `beamtalk_workspace_revert_tests:define_project_class/3`). Returns
%% `{ClassObj, Path}` where `ClassObj` is the `#beamtalk_object{}` class
%% object (element 4 = the class's own gen_server pid) `moveClass/2` and
%% `resolve_name/2`/`resolve_class_reference/2` expect.
define_project_class(Tmp, ClassNameBin) ->
    Path = filename:join(Tmp, binary_to_list(ClassNameBin) ++ ".bt"),
    Source = iolist_to_binary([<<"Object subclass: ">>, ClassNameBin, <<"\n  value => 1\n">>]),
    ok = file:write_file(Path, Source),
    {ok, _ClassNames} = beamtalk_repl_loader:reload_class_file(Path),
    ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, binary_to_list(Source)),
    ClassAtom = binary_to_atom(ClassNameBin, utf8),
    Pid = wait_for_class(ClassAtom, 50),
    {class_object_for(ClassAtom, Pid), Path}.

%% Define a `Supervisor subclass:` with a real `Counter` child so
%% `startSupervisor:`/`stopSupervisor:` drive the real OTP supervisor
%% behaviour codegen generates. Returns `{ClassObj, ClassAtom}`.
define_supervisor_class(Tmp, Unique) ->
    CounterName = "WiSupCounter" ++ Unique,
    CounterPath = filename:join(Tmp, CounterName ++ ".bt"),
    ok = file:write_file(
        CounterPath,
        iolist_to_binary([
            <<"Actor subclass: ">>,
            CounterName,
            <<"\n  state: value = 0\n\n  getValue => self.value\n">>
        ])
    ),
    {ok, _} = beamtalk_repl_loader:reload_class_file(CounterPath),
    _ = wait_for_class(list_to_atom(CounterName), 50),

    SupName = "WiSup" ++ Unique,
    SupPath = filename:join(Tmp, SupName ++ ".bt"),
    ok = file:write_file(
        SupPath,
        iolist_to_binary([
            <<"Supervisor subclass: ">>,
            SupName,
            <<"\n  class children -> List => #(">>,
            CounterName,
            <<")\n">>
        ])
    ),
    {ok, _} = beamtalk_repl_loader:reload_class_file(SupPath),
    SupAtom = list_to_atom(SupName),
    SupPid = wait_for_class(SupAtom, 50),
    {class_object_for(SupAtom, SupPid), SupAtom}.

%% Build the `#beamtalk_object{}` "class object" shape `is_class_object/1`
%% recognises (class tag ends in `" class"`) — the receiver shape
%% `moveClass/2`, `startSupervisor/1`, `stopSupervisor/1` expect.
class_object_for(ClassAtom, Pid) ->
    Tag = beamtalk_class_registry:class_object_tag(ClassAtom),
    Module = beamtalk_object_class:module_name_safe(Pid),
    #beamtalk_object{class = Tag, class_mod = Module, pid = Pid}.

%% Run `Fun/0` with a bare, locally-registered `beamtalk_workspace_sup`
%% (mirrors `beamtalk_workspace_interface_primitives_tests:start_bare_workspace_sup/0`)
%% — skips starting one if a real workspace_sup is already up (suite-ordering
%% safe), matching that module's existing convention.
with_bare_workspace_sup(Fun) ->
    case whereis(beamtalk_workspace_sup) of
        undefined ->
            {ok, SupPid} = supervisor:start_link(
                {local, beamtalk_workspace_sup}, ?MODULE, bare_sup
            ),
            try
                Fun()
            after
                stop_proc(SupPid)
            end;
        _ ->
            Fun()
    end.

%% supervisor init/1 callback used only by with_bare_workspace_sup/1.
init(bare_sup) ->
    {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, []}}.

fake_self(Pid) ->
    {beamtalk_object, 'WorkspaceInterface', 'bt@stdlib@workspace_interface', Pid}.

wait_for_class(_ClassAtom, 0) ->
    error(class_not_registered);
wait_for_class(ClassAtom, N) ->
    case beamtalk_runtime_api:whereis_class(ClassAtom) of
        Pid when is_pid(Pid) -> Pid;
        _ ->
            timer:sleep(20),
            wait_for_class(ClassAtom, N - 1)
    end.

stop_existing(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> stop_proc(Pid)
    end.

stop_proc(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end;
stop_proc(_) ->
    ok.

restore_home(false) -> os:unsetenv("HOME");
restore_home(OldHome) -> os:putenv("HOME", OldHome).

temp_dir() ->
    unicode:characters_to_list(beamtalk_file:'tempDirectory'()).
