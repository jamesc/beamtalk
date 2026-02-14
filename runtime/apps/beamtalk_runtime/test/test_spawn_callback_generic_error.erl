%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test helper that throws a generic error in on_actor_spawned/4.
%%% Uses throw (not error) to exercise the Kind:Reason catch-all clause
%%% in register_spawned/4, which is distinct from the error:undef and
%%% error:#beamtalk_error{} clauses tested by other helpers.

-module(test_spawn_callback_generic_error).

-include("beamtalk.hrl").

-export([on_actor_spawned/4]).

on_actor_spawned(_RegistryPid, _ActorPid, _ClassName, _ModuleName) ->
    Error = beamtalk_error:new(type_error, 'TestActor'),
    throw(Error).
