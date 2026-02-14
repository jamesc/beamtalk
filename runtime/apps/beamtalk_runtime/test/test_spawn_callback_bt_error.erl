%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test helper that raises a beamtalk_error in on_actor_spawned/4.

-module(test_spawn_callback_bt_error).

-include("beamtalk.hrl").

-export([on_actor_spawned/4]).

on_actor_spawned(_RegistryPid, _ActorPid, _ClassName, _ModuleName) ->
    Error = beamtalk_error:new(does_not_understand, 'TestActor'),
    error(Error).
