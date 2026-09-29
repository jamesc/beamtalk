%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app_hook_tests).

-include_lib("eunit/include/eunit.hrl").

%%% The actor registry is owned by beamtalk_runtime (BT-3633); the workspace
%%% app only installs the `actor_spawned_hook` for workspace_meta bookkeeping.

workspace_app_start_sets_hook_env_test() ->
    application:unset_env(beamtalk_runtime, actor_spawned_hook),
    {ok, SupPid} = beamtalk_workspace_app:start(normal, []),
    try
        ?assertEqual(
            {ok, {beamtalk_workspace_meta, on_actor_spawned}},
            application:get_env(beamtalk_runtime, actor_spawned_hook)
        )
    after
        OldTrapExit = process_flag(trap_exit, true),
        exit(SupPid, shutdown),
        receive
            {'EXIT', SupPid, _} -> ok
        after 1000 -> ok
        end,
        process_flag(trap_exit, OldTrapExit),
        application:unset_env(beamtalk_runtime, actor_spawned_hook)
    end.

workspace_app_stop_unsets_hook_env_test() ->
    application:set_env(
        beamtalk_runtime, actor_spawned_hook, {beamtalk_workspace_meta, on_actor_spawned}
    ),
    ok = beamtalk_workspace_app:stop(undefined),
    ?assertEqual(undefined, application:get_env(beamtalk_runtime, actor_spawned_hook)).
