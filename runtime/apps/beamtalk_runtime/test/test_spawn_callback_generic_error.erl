%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test helper that throws a generic error in on_actor_spawned/4.

-module(test_spawn_callback_generic_error).

-export([on_actor_spawned/4]).

on_actor_spawned(_RegistryPid, _ActorPid, _ClassName, _ModuleName) ->
    throw({test_error, "something went wrong"}).
