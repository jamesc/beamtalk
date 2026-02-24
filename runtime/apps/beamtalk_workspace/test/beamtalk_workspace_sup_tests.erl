%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace_sup module
%%%
%%% Tests workspace supervisor behavior, child specifications, and startup.

-module(beamtalk_workspace_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Test helper

test_config() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/tmp/test">>,
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

    %% Should have 10 children: workspace_meta, transcript_stream, system_dictionary, actor_registry, workspace_environment, workspace_bootstrap, repl_server, idle_monitor, actor_sup, session_sup
    ?assertEqual(10, length(ChildSpecs)).

children_ids_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    %% Verify all expected children are present
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(beamtalk_workspace_meta, Ids)),
    ?assert(lists:member(beamtalk_transcript_stream, Ids)),
    ?assert(lists:member(beamtalk_interface, Ids)),
    ?assert(lists:member(beamtalk_workspace_interface, Ids)),
    ?assert(lists:member(beamtalk_actor_registry, Ids)),
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
                bind_addr => {127, 0, 0, 1},
                web_port => undefined
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

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_interface],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual(
        {beamtalk_interface, start_link, [{local, 'Beamtalk'}, []]}, maps:get(start, Spec)
    ).

singletons_after_metadata_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    %% Singletons must come after workspace_meta (boot ordering)
    MetaIdx = index_of(beamtalk_workspace_meta, Ids),
    TranscriptIdx = index_of(beamtalk_transcript_stream, Ids),
    SysDictIdx = index_of(beamtalk_interface, Ids),
    ?assert(TranscriptIdx > MetaIdx),
    ?assert(SysDictIdx > MetaIdx).

%%% Bootstrap child spec tests (ADR 0019 Phase 2)

bootstrap_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_bootstrap],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    %% ProjectPath from test_config is <<"/tmp/test">> — passed for BT-739 module activation
    ?assertEqual(
        {beamtalk_workspace_bootstrap, start_link, [<<"/tmp/test">>]}, maps:get(start, Spec)
    ).

bootstrap_after_singletons_before_repl_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    BootstrapIdx = index_of(beamtalk_workspace_bootstrap, Ids),
    WorkspaceInterfaceIdx = index_of(beamtalk_workspace_interface, Ids),
    ReplServerIdx = index_of(beamtalk_repl_server, Ids),
    %% Bootstrap must come after all singletons but before REPL server
    ?assert(BootstrapIdx > WorkspaceInterfaceIdx),
    ?assert(BootstrapIdx < ReplServerIdx).

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

        %% Verify each child has correct ID and is alive
        ExpectedIds = [
            beamtalk_workspace_meta,
            beamtalk_transcript_stream,
            beamtalk_interface,
            beamtalk_actor_registry,
            beamtalk_workspace_interface,
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
        workspace_id => <<"test-defaults">>,
        project_path => <<"/tmp/test">>,
        tcp_port => 49152
    },
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),

    [MonitorSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],
    {beamtalk_idle_monitor, start_link, [MonitorConfig]} = maps:get(start, MonitorSpec),
    ?assertEqual(true, maps:get(enabled, MonitorConfig)).

default_max_idle_seconds_test() ->
    %% Config without max_idle_seconds should default to 4 hours
    Config = #{
        workspace_id => <<"test-defaults">>,
        project_path => <<"/tmp/test">>,
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

    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_interface],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual(
        {beamtalk_workspace_interface, start_link, [{local, 'Workspace'}]},
        maps:get(start, Spec)
    ).

%%% Registry interleaving test

registry_before_workspace_environment_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),

    Ids = [maps:get(id, S) || S <- ChildSpecs],
    RegistryIdx = index_of(beamtalk_actor_registry, Ids),
    WorkspaceEnvIdx = index_of(beamtalk_workspace_interface, Ids),
    %% Registry must come before WorkspaceInterface (it depends on registry)
    ?assert(RegistryIdx < WorkspaceEnvIdx).

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

    %% Every singleton should use {local, Name} registration
    SingletonIds = [
        beamtalk_transcript_stream, beamtalk_interface, beamtalk_workspace_interface
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
                bind_addr => {127, 0, 0, 1},
                web_port => undefined
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
