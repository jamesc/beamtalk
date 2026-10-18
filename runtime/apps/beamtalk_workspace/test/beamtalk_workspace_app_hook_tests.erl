%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app_hook_tests).

-include_lib("eunit/include/eunit.hrl").

%%% The actor registry is owned by beamtalk_runtime (BT-3633); the workspace
%%% app only installs the `actor_spawned_hook` for workspace_meta bookkeeping.

workspace_app_start_sets_hook_env_test() ->
    OldHook = application:get_env(beamtalk_runtime, actor_spawned_hook),
    application:unset_env(beamtalk_runtime, actor_spawned_hook),
    %% start/2 installs the hook before starting the supervisor tree, so the
    %% assertion holds even when another suite in the same EUnit VM left
    %% `beamtalk_workspace_app_sup` running (`already_started`); only a
    %% supervisor this test started is stopped again.
    StartResult = beamtalk_workspace_app:start(normal, []),
    try
        ?assertMatch({ok, _}, normalize_start(StartResult)),
        ?assertEqual(
            {ok, {beamtalk_workspace_meta, on_actor_spawned}},
            application:get_env(beamtalk_runtime, actor_spawned_hook)
        )
    after
        case StartResult of
            {ok, SupPid} ->
                OldTrapExit = process_flag(trap_exit, true),
                exit(SupPid, shutdown),
                receive
                    {'EXIT', SupPid, _} -> ok
                after 1000 -> ok
                end,
                process_flag(trap_exit, OldTrapExit);
            _ ->
                ok
        end,
        case OldHook of
            {ok, Hook} -> application:set_env(beamtalk_runtime, actor_spawned_hook, Hook);
            undefined -> application:unset_env(beamtalk_runtime, actor_spawned_hook)
        end
    end.

normalize_start({ok, Pid}) -> {ok, Pid};
normalize_start({error, {already_started, Pid}}) -> {ok, Pid};
normalize_start(Other) -> Other.

workspace_app_stop_unsets_hook_env_test() ->
    application:set_env(
        beamtalk_runtime, actor_spawned_hook, {beamtalk_workspace_meta, on_actor_spawned}
    ),
    ok = beamtalk_workspace_app:stop(undefined),
    ?assertEqual(undefined, application:get_env(beamtalk_runtime, actor_spawned_hook)).
