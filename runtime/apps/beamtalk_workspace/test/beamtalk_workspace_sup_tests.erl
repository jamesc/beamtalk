%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_sup_tests).

-moduledoc """
Unit tests for beamtalk_workspace_sup module

Tests workspace supervisor behavior, child specifications, and startup.
""".
-include_lib("eunit/include/eunit.hrl").

%% Exported for `logger:add_handler/3` to call back into
%% (`release_mode_include_compiler_logs_boot_warning_test` et al. below) —
%% eunit's auto-export parse transform only exports test functions, and a
%% logger handler callback is invoked as an ordinary cross-module call.
-export([log/2]).

%%====================================================================
%% Tests
%%====================================================================

%%% Test helpers

test_project_path() ->
    TmpDir = beamtalk_file:'tempDirectory'(),
    <<TmpDir/binary, "/bt-sup-test">>.

test_config() ->
    #{
        mode => workspace,
        workspace_id => <<"test123">>,
        project_path => test_project_path(),
        tcp_port => 49152,
        % Disable for testing
        auto_cleanup => false
    }.

%%% Supervisor flags tests

supervisor_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Should use one_for_one strategy
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

supervisor_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Should allow 5 restarts in 10 seconds
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(10, maps:get(period, SupFlags)).

%%% Child specification tests

children_count_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Should have 13 children: workspace_meta, workspace_changelog,
    %% workspace_signature_store, workspace_shape_store,
    %% workspace_shape_recheck_worker, workspace_findings_store,
    %% transcript_stream, actor_registry, workspace_bootstrap, repl_server,
    %% idle_monitor, actor_sup, session_sup.
    %% BeamtalkInterface and WorkspaceInterface are value singletons (no gen_server).
    %% the class_events / bindings_events / flush_events pub/sub
    %% gen_servers were retired — those push streams now ride the SystemAnnouncer
    %% bus (`beamtalk_announcements`, supervised under `beamtalk_runtime_sup`).
    %% ADR 0105 Phase 1: workspace_signature_store added.
    %% ADR 0105 Phase 1: workspace_findings_store added.
    %% ADR 0105 Phase 2: workspace_shape_store and
    %% workspace_shape_recheck_worker added.
    %% ADR 0108 hot-reload re-check trigger: beamtalk_alias_xref added.
    ?assertEqual(14, length(ChildSpecs)).

children_ids_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Verify all expected children are present.
    %% BeamtalkInterface and WorkspaceInterface are value singletons —
    %% they are NOT gen_server children of this supervisor.
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(beamtalk_workspace_meta, Ids)),
    ?assert(lists:member(beamtalk_workspace_changelog, Ids)),
    ?assert(lists:member(beamtalk_workspace_signature_store, Ids)),
    ?assert(lists:member(beamtalk_workspace_shape_store, Ids)),
    ?assert(lists:member(beamtalk_workspace_shape_recheck_worker, Ids)),
    ?assert(lists:member(beamtalk_workspace_findings_store, Ids)),
    %% ADR 0108 hot-reload re-check trigger.
    ?assert(lists:member(beamtalk_alias_xref, Ids)),
    ?assert(lists:member(beamtalk_transcript_stream, Ids)),
    ?assertNot(lists:member('bt@stdlib@beamtalk_interface', Ids)),
    ?assertNot(lists:member('bt@stdlib@workspace_interface', Ids)),
    ?assert(lists:member(beamtalk_actor_registry, Ids)),
    %% class_events / bindings_events / flush_events retired.
    ?assertNot(lists:member(beamtalk_class_events, Ids)),
    ?assertNot(lists:member(beamtalk_bindings_events, Ids)),
    ?assertNot(lists:member(beamtalk_flush_events, Ids)),
    ?assert(lists:member(beamtalk_repl_server, Ids)),
    ?assert(lists:member(beamtalk_idle_monitor, Ids)),
    ?assert(lists:member(beamtalk_actor_sup, Ids)),
    ?assert(lists:member(beamtalk_session_sup, Ids)),
    ?assert(lists:member(beamtalk_workspace_bootstrap, Ids)).

workspace_meta_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Find workspace_meta child spec
    [MetaSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_meta],

    %% Should be a worker, permanent, with correct module
    ?assertEqual(worker, maps:get(type, MetaSpec)),
    ?assertEqual(permanent, maps:get(restart, MetaSpec)),

    %% Start function should use beamtalk_workspace_meta:start_link with config map
    {Mod, Fun, [Config]} = maps:get(start, MetaSpec),
    ?assertEqual(beamtalk_workspace_meta, Mod),
    ?assertEqual(start_link, Fun),
    ?assert(maps:is_key(workspace_id, Config)).

actor_registry_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Find actor_registry child spec
    [RegistrySpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_actor_registry],
    ?assertEqual(worker, maps:get(type, RegistrySpec)),
    ?assertEqual(permanent, maps:get(restart, RegistrySpec)),
    ?assertEqual(
        {beamtalk_repl_actors, start_link, [registered]},
        maps:get(start, RegistrySpec)
    ).

repl_server_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Find repl_server child spec
    [ReplSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_repl_server],
    ?assertEqual(worker, maps:get(type, ReplSpec)),
    ?assertEqual(permanent, maps:get(restart, ReplSpec)),
    ?assertEqual(
        {beamtalk_repl_server, start_link, [
            #{
                port => 49152,
                workspace_id => <<"test123">>,
                bind_addr => {127, 0, 0, 1}
            }
        ]},
        maps:get(start, ReplSpec)
    ).

idle_monitor_spec_test() ->
    Config = test_config(),
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    %% Find idle_monitor child specs
    MonitorSpecs = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],

    %% With auto_cleanup => false, idle_monitor should still be in child specs
    %% (supervisor filters at runtime, not init time)
    ?assertEqual(1, length(MonitorSpecs)),

    [MonitorSpec] = MonitorSpecs,
    ?assertEqual(worker, maps:get(type, MonitorSpec)),
    ?assertEqual(permanent, maps:get(restart, MonitorSpec)).

actor_sup_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Find actor_sup child spec
    [ActorSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_actor_sup],

    %% Should be a supervisor, permanent
    ?assertEqual(supervisor, maps:get(type, ActorSupSpec)),
    ?assertEqual(permanent, maps:get(restart, ActorSupSpec)),
    ?assertEqual({beamtalk_actor_sup, start_link, []}, maps:get(start, ActorSupSpec)).

session_sup_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Find session_sup child spec
    [SessionSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_session_sup],

    %% Should be a supervisor, permanent
    ?assertEqual(supervisor, maps:get(type, SessionSupSpec)),
    ?assertEqual(permanent, maps:get(restart, SessionSupSpec)),
    ?assertEqual({beamtalk_session_sup, start_link, []}, maps:get(start, SessionSupSpec)).

%%% Singleton child spec tests (ADR 0010 Phase 2)

transcript_stream_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_transcript_stream],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual(
        {beamtalk_transcript_stream, start_link, [{local, 'Transcript'}, 1000]},
        maps:get(start, Spec)
    ).

system_dictionary_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% BeamtalkInterface is a value singleton — it must NOT appear as a supervisor child.
    Specs = [S || S <- ChildSpecs, maps:get(id, S) == 'bt@stdlib@beamtalk_interface'],
    ?assertEqual([], Specs).

singletons_after_metadata_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    %% Actor singletons must come after workspace_meta (boot ordering)
    MetaIdx = index_of(beamtalk_workspace_meta, Ids),
    TranscriptIdx = index_of(beamtalk_transcript_stream, Ids),
    ?assert(TranscriptIdx > MetaIdx).

%%% Bootstrap child spec tests (ADR 0019 Phase 2)

bootstrap_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_bootstrap],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    %% ProjectPath from test_config is propagated to bootstrap start args (module activation)
    ?assertEqual(
        {beamtalk_workspace_bootstrap, start_link, [test_project_path()]}, maps:get(start, Spec)
    ).

bootstrap_after_singletons_before_repl_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    BootstrapIdx = index_of(beamtalk_workspace_bootstrap, Ids),
    ActorRegistryIdx = index_of(beamtalk_actor_registry, Ids),
    ReplServerIdx = index_of(beamtalk_repl_server, Ids),
    %% Bootstrap must come after actor registry (and all actor singletons) but before REPL
    ?assert(BootstrapIdx > ActorRegistryIdx),
    ?assert(BootstrapIdx < ReplServerIdx).

session_sup_before_repl_server_test() ->
    %% The session supervisor MUST start before the REPL server. repl_server's
    %% init/1 binds the cowboy `/ws` listener and writes the port file (the CLI's
    %% readiness signal) before returning; if session_sup is not yet up when a
    %% client connects, beamtalk_ws_handler:create_session exits with `noproc` and
    %% the connection is dropped, surfacing as the flaky "port accepting TCP but
    %% WebSocket health check failed" CI failure.
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    SessionSupIdx = index_of(beamtalk_session_sup, Ids),
    ReplServerIdx = index_of(beamtalk_repl_server, Ids),
    ?assert(SessionSupIdx < ReplServerIdx).

%%% Helpers

index_of(Elem, List) ->
    index_of(Elem, List, 1).

index_of(_Elem, [], _N) ->
    not_found;
index_of(Elem, [Elem | _], N) ->
    N;
index_of(Elem, [_ | Rest], N) ->
    index_of(Elem, Rest, N + 1).

%%% Startup smoke tests

all_children_alive_test() ->
    %% Ensure runtime is started (workspace depends on it)
    {ok, _} = application:ensure_all_started(beamtalk_runtime),

    %% The repl_server child boots a cowboy listener (`cowboy:start_clear`), which
    %% needs ranch/cowboy already running. When this suite runs as an isolated
    %% module (`rebar3 eunit --module=…`) the beamtalk_workspace app — and thus its
    %% cowboy dependency — is not started, so the child would fail with
    %% `{noproc, ranch_sup …}`. Starting cowboy here makes the single-module run
    %% deterministic.
    {ok, _} = application:ensure_all_started(cowboy),

    %% Set trap_exit before start_link
    OldTrap = process_flag(trap_exit, true),

    %% Use ephemeral port to avoid flakiness from port conflicts
    Config0 = test_config(),
    Config = Config0#{tcp_port => 0},

    %% Handle already-started supervisor (registered name)
    {Sup, WeStarted} =
        case beamtalk_workspace_sup:start_link(Config) of
            {ok, Pid} -> {Pid, true};
            {error, {already_started, Pid}} -> {Pid, false}
        end,

    %% If supervisor was already running, give children time to stabilize
    case WeStarted of
        false -> timer:sleep(100);
        true -> ok
    end,

    try
        %% Get all children
        Children = supervisor:which_children(Sup),

        %% Verify each child has correct ID and is alive.
        %% BeamtalkInterface and WorkspaceInterface are value singletons — not children.
        %% class_events / bindings_events / flush_events retired (those
        %% push streams now ride the SystemAnnouncer bus).
        ExpectedIds = [
            beamtalk_workspace_meta,
            beamtalk_workspace_changelog,
            %% ADR 0105 Phase 1.
            beamtalk_workspace_signature_store,
            %% ADR 0105 Phase 2.
            beamtalk_workspace_shape_store,
            beamtalk_workspace_shape_recheck_worker,
            %% ADR 0105 Phase 1.
            beamtalk_workspace_findings_store,
            %% ADR 0108 hot-reload re-check trigger.
            beamtalk_alias_xref,
            beamtalk_transcript_stream,
            beamtalk_actor_registry,
            beamtalk_workspace_bootstrap,
            beamtalk_repl_server,
            beamtalk_idle_monitor,
            beamtalk_actor_sup,
            beamtalk_session_sup
        ],
        ActualIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
        ?assertEqual(lists:sort(ExpectedIds), lists:sort(ActualIds)),

        %% Verify each child process is alive
        lists:foreach(
            fun({_Id, ChildPid, _Type, _Modules}) ->
                ?assert(is_process_alive(ChildPid))
            end,
            Children
        )
    after
        %% Only shut down supervisor if we started it
        case WeStarted of
            true ->
                exit(Sup, shutdown),
                receive
                    {'EXIT', Sup, _} -> ok
                after 1000 -> ok
                end;
            false ->
                ok
        end,
        process_flag(trap_exit, OldTrap)
    end.

%%% Default config value tests

default_auto_cleanup_test() ->
    %% Config without auto_cleanup should default to true
    Config = #{
        mode => workspace,
        workspace_id => <<"test-defaults">>,
        project_path => test_project_path(),
        tcp_port => 49152
    },
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [MonitorSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],
    {beamtalk_idle_monitor, start_link, [MonitorConfig]} = maps:get(start, MonitorSpec),
    ?assertEqual(true, maps:get(enabled, MonitorConfig)).

default_max_idle_seconds_test() ->
    %% Config without max_idle_seconds should default to 4 hours
    Config = #{
        mode => workspace,
        workspace_id => <<"test-defaults">>,
        project_path => test_project_path(),
        tcp_port => 49152
    },
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [MonitorSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],
    {beamtalk_idle_monitor, start_link, [MonitorConfig]} = maps:get(start, MonitorSpec),
    ?assertEqual(3600 * 4, maps:get(max_idle_seconds, MonitorConfig)).

idle_monitor_config_propagation_test() ->
    %% Custom max_idle_seconds should be propagated
    Config0 = test_config(),
    Config = Config0#{max_idle_seconds => 1800},
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [MonitorSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],
    {beamtalk_idle_monitor, start_link, [MonitorConfig]} = maps:get(start, MonitorSpec),
    ?assertEqual(1800, maps:get(max_idle_seconds, MonitorConfig)).

%%% Shutdown configuration tests

actor_sup_shutdown_infinity_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [ActorSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_actor_sup],
    ?assertEqual(infinity, maps:get(shutdown, ActorSupSpec)).

session_sup_shutdown_infinity_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [SessionSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_session_sup],
    ?assertEqual(infinity, maps:get(shutdown, SessionSupSpec)).

%%% Workspace interface child spec test

workspace_environment_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% WorkspaceInterface is a value singleton — must NOT appear as a supervisor child.
    Specs = [S || S <- ChildSpecs, maps:get(id, S) == 'bt@stdlib@workspace_interface'],
    ?assertEqual([], Specs).

%%% Registry interleaving test

registry_before_bootstrap_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    RegistryIdx = index_of(beamtalk_actor_registry, Ids),
    BootstrapIdx = index_of(beamtalk_workspace_bootstrap, Ids),
    %% Registry must come before bootstrap (bootstrap depends on registry)
    ?assert(RegistryIdx < BootstrapIdx).

%%% File logger tests

file_logger_disabled_by_env_test() ->
    %% BEAMTALK_NO_FILE_LOG=1 should skip file logger setup entirely
    OldVal = os:getenv("BEAMTALK_NO_FILE_LOG"),
    try
        os:putenv("BEAMTALK_NO_FILE_LOG", "1"),
        %% init should succeed without adding a file handler
        {ok, {_SupFlags, _ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
        ok
    after
        case OldVal of
            false -> os:unsetenv("BEAMTALK_NO_FILE_LOG");
            Val -> os:putenv("BEAMTALK_NO_FILE_LOG", Val)
        end
    end.

%%% Bootstrap child spec details test

bootstrap_spec_modules_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_bootstrap],
    ?assertEqual([beamtalk_workspace_bootstrap], maps:get(modules, Spec)),
    ?assertEqual(5000, maps:get(shutdown, Spec)).

%%% Singleton child spec generation tests

singleton_specs_have_local_registration_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Actor singletons use {local, Name} registration.
    %% BeamtalkInterface and WorkspaceInterface are value singletons — not children.
    SingletonIds = [
        beamtalk_transcript_stream
    ],
    lists:foreach(
        fun(Id) ->
            [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == Id],
            {_Mod, start_link, [{local, _Name} | _Args]} = maps:get(start, Spec),
            ok
        end,
        SingletonIds
    ).

%%% REPL server config test

repl_server_config_test() ->
    Config0 = test_config(),
    Config = Config0#{tcp_port => 12345, workspace_id => <<"ws-custom">>},
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [ReplSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_repl_server],
    ?assertEqual(
        {beamtalk_repl_server, start_link, [
            #{
                port => 12345,
                workspace_id => <<"ws-custom">>,
                bind_addr => {127, 0, 0, 1}
            }
        ]},
        maps:get(start, ReplSpec)
    ).

%%% Bind address config test

repl_server_custom_bind_addr_test() ->
    Config0 = test_config(),
    Config = Config0#{bind_addr => {192, 168, 1, 5}},
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [ReplSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_repl_server],
    {beamtalk_repl_server, start_link, [ReplConfig]} = maps:get(start, ReplSpec),
    ?assertEqual({192, 168, 1, 5}, maps:get(bind_addr, ReplConfig)).

%%% Workspace meta config test

workspace_meta_config_test() ->
    Config = #{
        mode => workspace,
        workspace_id => <<"meta-test">>,
        project_path => <<"/home/test/project">>,
        tcp_port => 5555,
        auto_cleanup => false
    },
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [MetaSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_meta],
    {beamtalk_workspace_meta, start_link, [MetaConfig]} = maps:get(start, MetaSpec),
    ?assertEqual(<<"meta-test">>, maps:get(workspace_id, MetaConfig)),
    ?assertEqual(<<"/home/test/project">>, maps:get(project_path, MetaConfig)),
    ?assertEqual(5555, maps:get(repl_port, MetaConfig)),
    ?assert(is_integer(maps:get(created_at, MetaConfig))).

%%% Mode tests (ADR 0125 §1.4)
%%%
%%% `mode => run | workspace | release` selects the child set. Each mode's
%%% exact child-id list is asserted below, so a child added to (or dropped
%%% from) a mode shows up here.

%% Ids common to every mode, in start order: meta, changelog, the actor
%% singletons (beamtalk_workspace_config:singletons/0), the actor registry,
%% bootstrap, actor_sup.
base_child_ids() ->
    [beamtalk_workspace_meta, beamtalk_workspace_changelog] ++
        [maps:get(module, S) || S <- beamtalk_workspace_config:singletons()] ++
        [beamtalk_actor_registry, beamtalk_workspace_bootstrap, beamtalk_actor_sup].

live_development_child_ids() ->
    [
        beamtalk_workspace_signature_store,
        beamtalk_workspace_shape_store,
        beamtalk_alias_xref,
        beamtalk_workspace_shape_recheck_worker,
        beamtalk_workspace_findings_store
    ].

console_child_ids() ->
    [beamtalk_session_sup, beamtalk_repl_server].

child_ids(Config) ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),
    [maps:get(id, S) || S <- ChildSpecs].

child_spec(Id, Config) ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),
    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == Id],
    Spec.

run_mode_config() ->
    #{
        mode => run,
        workspace_id => <<"run-mode-test">>,
        project_path => test_project_path()
        %% tcp_port intentionally omitted — not required in run mode
    }.

release_mode_config() ->
    #{
        mode => release,
        workspace_id => <<"release-mode-test">>,
        project_path => undefined
        %% tcp_port omitted — not required without a console
    }.

release_console_config() ->
    (release_mode_config())#{console => true, tcp_port => 49153}.

%% A release-mode init/1 records release capabilities in a persistent_term;
%% restore the default so later tests in this VM are unaffected.
with_capabilities_restored(Fun) ->
    try
        Fun()
    after
        beamtalk_capability:clear()
    end.

run_mode_child_ids_test() ->
    %% Run mode: the base set only — no REPL listener, no idle monitor, no
    %% ADR 0105 / 0108 live-development stores.
    ?assertEqual(base_child_ids(), child_ids(run_mode_config())).

workspace_mode_child_ids_test() ->
    ?assertEqual(
        base_child_ids() ++ live_development_child_ids() ++ console_child_ids() ++
            [beamtalk_idle_monitor],
        child_ids(test_config())
    ).

release_mode_child_ids_test() ->
    %% Release without a console: the base set only. Never the idle monitor
    %% (it calls init:stop/0), never the live-development stores.
    with_capabilities_restored(fun() ->
        ?assertEqual(base_child_ids(), child_ids(release_mode_config()))
    end).

release_mode_with_console_child_ids_test() ->
    %% `console => true` adds the REPL listener — and nothing else.
    with_capabilities_restored(fun() ->
        ?assertEqual(base_child_ids() ++ console_child_ids(), child_ids(release_console_config()))
    end).

release_mode_console_preserves_session_sup_ordering_test() ->
    %% session_sup must start before repl_server in every mode that has them.
    with_capabilities_restored(fun() ->
        Ids = child_ids(release_console_config()),
        ?assert(
            string:str(Ids, [beamtalk_session_sup]) < string:str(Ids, [beamtalk_repl_server])
        )
    end).

release_mode_never_idle_monitor_test() ->
    %% Even with auto_cleanup set, a release never starts the idle monitor.
    with_capabilities_restored(fun() ->
        Config = (release_console_config())#{auto_cleanup => true, max_idle_seconds => 1},
        ?assertNot(lists:member(beamtalk_idle_monitor, child_ids(Config)))
    end).

release_mode_console_requires_tcp_port_test() ->
    with_capabilities_restored(fun() ->
        ?assertError(
            {bad_config, missing_tcp_port_for_repl},
            beamtalk_workspace_sup:init((release_mode_config())#{console => true})
        )
    end).

release_mode_ignores_console_key_outside_release_test() ->
    %% `console` is a release-mode option: run mode never starts a listener.
    Ids = child_ids((run_mode_config())#{console => true, tcp_port => 49154}),
    ?assertNot(lists:member(beamtalk_repl_server, Ids)).

release_mode_records_capabilities_test() ->
    with_capabilities_restored(fun() ->
        _ = child_ids(release_mode_config()),
        ?assertEqual(
            #{mode => release, include_compiler => false}, beamtalk_capability:current()
        ),
        _ = child_ids((release_mode_config())#{include_compiler => true}),
        ?assertEqual(
            #{mode => release, include_compiler => true}, beamtalk_capability:current()
        )
    end).

workspace_mode_records_capabilities_test() ->
    with_capabilities_restored(fun() ->
        _ = child_ids(test_config()),
        ?assertMatch(#{mode := workspace}, beamtalk_capability:current())
    end).

%% ADR 0125 §1.5: a release built with `include-compiler` must log a
%% warning naming the three risks every time it boots, not just once at
%% build time — an operator reading logs must see it even if they weren't
%% the one who set the flag.
%% `{timeout, 10, ...}`: EUnit's own default 5s timetrap would otherwise
%% race the 3s `receive_include_compiler_warning/1` wait below (plus this
%% test's own setup) under the full suite's scheduler contention — this
%% raises the *test's* budget, independent of that wait.
release_mode_include_compiler_logs_boot_warning_test_() ->
    {timeout, 10, fun() ->
        with_capabilities_restored(fun() ->
            Parent = self(),
            HandlerId = release_include_compiler_warning_test_handler,
            %% `apps/beamtalk_runtime/test/sys.config` sets the *primary*
            %% logger level to `error` for clean test-suite output — a
            %% WARNING event never reaches any handler, including this
            %% test's own, unless the primary level is raised first. Same
            %% pattern (and restore-to-`error`, not the pre-call value)
            %% `beamtalk_shape_migration_tests` already uses for its own
            %% stray-migration-warning assertions.
            logger:set_primary_config(level, all),
            ok = logger:add_handler(HandlerId, ?MODULE, #{config => Parent}),
            try
                _ = child_ids((release_mode_config())#{include_compiler => true}),
                ?assert(receive_include_compiler_warning(500))
            after
                _ = logger:remove_handler(HandlerId),
                logger:set_primary_config(level, error)
            end
        end)
    end}.

release_mode_without_include_compiler_logs_no_boot_warning_test() ->
    with_capabilities_restored(fun() ->
        Parent = self(),
        HandlerId = release_no_include_compiler_warning_test_handler,
        %% Raise the primary level the same way the positive-case test
        %% does, so this negative assertion actually exercises "no WARNING
        %% is logged" rather than passing vacuously because the ambient
        %% `error`-level primary config already drops it either way.
        logger:set_primary_config(level, all),
        ok = logger:add_handler(HandlerId, ?MODULE, #{config => Parent}),
        try
            _ = child_ids(release_mode_config()),
            ?assertNot(receive_include_compiler_warning(200))
        after
            _ = logger:remove_handler(HandlerId),
            logger:set_primary_config(level, error)
        end
    end).

%% Minimal logger handler callback (ADR 0125 §1.5 boot-warning test only):
%% forwards every log event's formatted message to the test process.
-doc "logger handler callback — forwards formatted messages to `Config`.".
log(#{msg := Msg} = Event, #{config := Parent}) ->
    Parent ! {sup_log_event, format_msg(Msg, Event)},
    ok.

format_msg({string, Str}, _Event) ->
    Str;
format_msg({report, Report}, _Event) when is_map(Report) ->
    io_lib:format("~p", [Report]);
format_msg({Format, Args}, _Event) ->
    io_lib:format(Format, Args);
format_msg(Msg, _Event) ->
    io_lib:format("~p", [Msg]).

receive_include_compiler_warning(Timeout) ->
    receive_log_message_containing("include-compiler", Timeout).

%% Shared substring-matching wait, parameterised by the needle — one
%% implementation for every boot-warning test in this suite (ADR 0125 §1.5's
%% include-compiler warning and §1.6's non-loopback-bind warning below),
%% not a second near-identical copy per warning (CLAUDE.md's
%% no-duplicate-implementations rule).
receive_log_message_containing(Needle, Timeout) ->
    receive
        {sup_log_event, Msg} ->
            case string:find(unicode:characters_to_list(Msg), Needle) of
                nomatch -> receive_log_message_containing(Needle, Timeout);
                _ -> true
            end
    after Timeout ->
        false
    end.

%% ADR 0125 §1.6: a release console bound to a non-loopback address must log
%% a boot warning naming ADR 0058 and the reverse-proxy alternative — never
%% silently. Not a refusal: `child_ids/1` (and so `init/1`) must still
%% succeed with the REPL listener child present.
release_mode_non_loopback_bind_logs_boot_warning_test_() ->
    {timeout, 10, fun() ->
        with_capabilities_restored(fun() ->
            Parent = self(),
            HandlerId = release_non_loopback_bind_warning_test_handler,
            logger:set_primary_config(level, all),
            ok = logger:add_handler(HandlerId, ?MODULE, #{config => Parent}),
            try
                Config = (release_console_config())#{bind_addr => {0, 0, 0, 0}},
                Ids = child_ids(Config),
                ?assert(lists:member(beamtalk_repl_server, Ids)),
                ?assert(receive_log_message_containing("non-loopback", 500))
            after
                _ = logger:remove_handler(HandlerId),
                logger:set_primary_config(level, error)
            end
        end)
    end}.

release_mode_loopback_bind_logs_no_boot_warning_test() ->
    with_capabilities_restored(fun() ->
        Parent = self(),
        HandlerId = release_loopback_bind_no_warning_test_handler,
        logger:set_primary_config(level, all),
        ok = logger:add_handler(HandlerId, ?MODULE, #{config => Parent}),
        try
            Config = (release_console_config())#{bind_addr => {127, 0, 0, 1}},
            _ = child_ids(Config),
            ?assertNot(receive_log_message_containing("non-loopback", 200))
        after
            _ = logger:remove_handler(HandlerId),
            logger:set_primary_config(level, error)
        end
    end).

%% Only `mode => release, console => true` is checked — a workspace-mode
%% node bound non-loopback (a CLI-driven development node) logs nothing.
workspace_mode_non_loopback_bind_logs_no_boot_warning_test() ->
    Parent = self(),
    HandlerId = workspace_non_loopback_bind_no_warning_test_handler,
    logger:set_primary_config(level, all),
    ok = logger:add_handler(HandlerId, ?MODULE, #{config => Parent}),
    try
        Config = (test_config())#{bind_addr => {0, 0, 0, 0}},
        _ = child_ids(Config),
        ?assertNot(receive_log_message_containing("non-loopback", 200))
    after
        _ = logger:remove_handler(HandlerId),
        logger:set_primary_config(level, error)
    end.

run_mode_no_tcp_port_required_test() ->
    %% Starting in run mode with no tcp_port should succeed.
    ?assert(length(child_ids(run_mode_config())) > 0).

run_mode_required_children_present_test() ->
    Ids = child_ids(run_mode_config()),
    ?assert(lists:member(beamtalk_workspace_meta, Ids)),
    ?assert(lists:member(beamtalk_workspace_bootstrap, Ids)),
    ?assert(lists:member(beamtalk_actor_sup, Ids)),
    ?assert(lists:member(beamtalk_actor_registry, Ids)).

%%% Mode passed to workspace_meta

meta_mode(Config) ->
    {beamtalk_workspace_meta, start_link, [MetaConfig]} =
        maps:get(start, child_spec(beamtalk_workspace_meta, Config)),
    maps:get(mode, MetaConfig).

run_mode_meta_gets_mode_run_test() ->
    ?assertEqual(run, meta_mode(run_mode_config())).

workspace_mode_meta_gets_mode_workspace_test() ->
    ?assertEqual(workspace, meta_mode(test_config())).

release_mode_meta_gets_mode_release_test() ->
    with_capabilities_restored(fun() ->
        ?assertEqual(release, meta_mode(release_mode_config()))
    end).

%%% ChangeLog workspace id (memory-only outside workspace mode)

changelog_workspace_id(Config) ->
    {beamtalk_workspace_changelog, start_link, [#{workspace_id := Id}]} =
        maps:get(start, child_spec(beamtalk_workspace_changelog, Config)),
    Id.

run_mode_changelog_memory_only_test() ->
    ?assertEqual(undefined, changelog_workspace_id(run_mode_config())).

workspace_mode_changelog_on_disk_test() ->
    ?assertEqual(<<"test123">>, changelog_workspace_id(test_config())).

release_mode_changelog_memory_only_test() ->
    with_capabilities_restored(fun() ->
        ?assertEqual(undefined, changelog_workspace_id(release_mode_config()))
    end).

%%% Config validation

workspace_mode_missing_tcp_port_fails_fast_test() ->
    %% Workspace mode without tcp_port must fail fast, not silently pass
    %% undefined into the REPL server child spec.
    Config = #{
        mode => workspace,
        workspace_id => <<"fail-fast-test">>,
        project_path => test_project_path()
    },
    ?assertError(
        {bad_config, missing_tcp_port_for_repl},
        beamtalk_workspace_sup:init(Config)
    ).

missing_mode_fails_fast_test() ->
    %% `mode` is required — there is no default.
    ?assertError(
        {bad_config, missing_mode},
        beamtalk_workspace_sup:init(maps:remove(mode, test_config()))
    ).

invalid_mode_fails_fast_test() ->
    ?assertError(
        {bad_config, {invalid_mode, repl}},
        beamtalk_workspace_sup:init((test_config())#{mode => repl})
    ).

removed_repl_key_fails_fast_test() ->
    %% No transitional `repl => boolean()` clause: a stale caller fails loudly
    %% instead of being silently mapped to a mode.
    ?assertError(
        {bad_config, {removed_key, repl, use_mode}},
        beamtalk_workspace_sup:init((run_mode_config())#{repl => false})
    ).
