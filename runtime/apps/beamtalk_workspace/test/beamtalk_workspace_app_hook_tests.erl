%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app_hook_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ADR 0150 (BT-4950): the workspace app no longer installs an
%%% `actor_spawned_hook` into beamtalk_runtime. `beamtalk_workspace_meta` learns
%%% about spawned actors from the `ActorSpawned` announcement
%%% (see beamtalk_workspace_meta_tests).

workspace_app_start_installs_no_spawn_hook_test() ->
    OldHook = application:get_env(beamtalk_runtime, actor_spawned_hook),
    application:unset_env(beamtalk_runtime, actor_spawned_hook),
    StartResult = beamtalk_workspace_app:start(normal, []),
    try
        ?assertMatch({ok, _}, normalize_start(StartResult)),
        ?assertEqual(undefined, application:get_env(beamtalk_runtime, actor_spawned_hook))
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
