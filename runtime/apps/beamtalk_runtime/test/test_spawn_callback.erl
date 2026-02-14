%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test helper for actor spawn callback tests.
%%% Implements on_actor_spawned/4 and sends a message to a registered
%%% test process so tests can verify the callback was invoked.

-module(test_spawn_callback).

-export([on_actor_spawned/4]).

on_actor_spawned(RegistryPid, ActorPid, ClassName, ModuleName) ->
    case whereis(spawn_callback_test) of
        undefined -> ok;
        TestPid ->
            TestPid ! {callback_invoked, RegistryPid, ActorPid, ClassName, ModuleName}
    end,
    ok.
