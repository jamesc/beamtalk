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
        auto_cleanup => false  % Disable for testing
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
    ?assert(lists:member(beamtalk_system_dictionary, Ids)),
    ?assert(lists:member(beamtalk_workspace_environment, Ids)),
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
    ?assertEqual({beamtalk_repl_actors, start_link, [registered]}, 
                 maps:get(start, RegistrySpec)).

repl_server_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Find repl_server child spec
    [ReplSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_repl_server],
    ?assertEqual(worker, maps:get(type, ReplSpec)),
    ?assertEqual(permanent, maps:get(restart, ReplSpec)),
    ?assertEqual({beamtalk_repl_server, start_link, [#{port => 49152, workspace_id => <<"test123">>}]}, 
                 maps:get(start, ReplSpec)).

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
    ?assertEqual({beamtalk_transcript_stream, start_link, [{local, 'Transcript'}, 1000]}, maps:get(start, Spec)).

system_dictionary_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_system_dictionary],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual({beamtalk_system_dictionary, start_link, [{local, 'Beamtalk'}, []]}, maps:get(start, Spec)).

singletons_after_metadata_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    Ids = [maps:get(id, S) || S <- ChildSpecs],
    %% Singletons must come after workspace_meta (boot ordering)
    MetaIdx = index_of(beamtalk_workspace_meta, Ids),
    TranscriptIdx = index_of(beamtalk_transcript_stream, Ids),
    SysDictIdx = index_of(beamtalk_system_dictionary, Ids),
    ?assert(TranscriptIdx > MetaIdx),
    ?assert(SysDictIdx > MetaIdx).

%%% Bootstrap child spec tests (ADR 0019 Phase 2)

bootstrap_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    [Spec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_bootstrap],
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual({beamtalk_workspace_bootstrap, start_link, []}, maps:get(start, Spec)).

bootstrap_after_singletons_before_repl_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    Ids = [maps:get(id, S) || S <- ChildSpecs],
    BootstrapIdx = index_of(beamtalk_workspace_bootstrap, Ids),
    WorkspaceActorIdx = index_of(beamtalk_workspace_environment, Ids),
    ReplServerIdx = index_of(beamtalk_repl_server, Ids),
    %% Bootstrap must come after all singletons but before REPL server
    ?assert(BootstrapIdx > WorkspaceActorIdx),
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
    {Sup, WeStarted} = case beamtalk_workspace_sup:start_link(Config) of
        {ok, Pid} -> {Pid, true};
        {error, {already_started, Pid}} -> {Pid, false}
    end,

    try
        %% Get all children
        Children = supervisor:which_children(Sup),

        %% Expected: 10 children (workspace_meta, transcript_stream, system_dictionary,
        %% actor_registry, workspace_environment, workspace_bootstrap, repl_server, idle_monitor, actor_sup, session_sup)
        ?assertEqual(10, length(Children)),

        %% Verify each child has correct ID and is alive
        ExpectedIds = [
            beamtalk_workspace_meta,
            beamtalk_transcript_stream,
            beamtalk_system_dictionary,
            beamtalk_actor_registry,
            beamtalk_workspace_environment,
            beamtalk_workspace_bootstrap,
            beamtalk_repl_server,
            beamtalk_idle_monitor,
            beamtalk_actor_sup,
            beamtalk_session_sup
        ],
        ActualIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
        ?assertEqual(lists:sort(ExpectedIds), lists:sort(ActualIds)),

        %% Verify each child process is alive
        lists:foreach(fun({_Id, ChildPid, _Type, _Modules}) ->
            ?assert(is_process_alive(ChildPid))
        end, Children)
    after
        %% Only shut down supervisor if we started it
        case WeStarted of
            true ->
                exit(Sup, shutdown),
                receive {'EXIT', Sup, _} -> ok after 1000 -> ok end;
            false ->
                ok
        end,
        process_flag(trap_exit, OldTrap)
    end.
