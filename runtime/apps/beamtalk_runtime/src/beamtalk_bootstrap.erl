%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bootstrap the runtime environment.
%%%
%%% **DDD Context:** Runtime
%%%
%%% BT-446: Bootstrap no longer creates class processes for ProtoObject,
%%% Object, and Actor. Those are now registered by their compiled stdlib
%%% modules' on_load via beamtalk_stdlib:load_compiled_stdlib_modules().
%%%
%%% Bootstrap only ensures pg (process group) is started, which is needed
%%% by the class registry (beamtalk_object_class) before any classes load.
-module(beamtalk_bootstrap).

-export([start_link/0, init/1]).

%% @doc Start the bootstrap process.
%%
%% This is typically called during application startup to ensure pg is
%% running before class modules load.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%% @doc Initialize the runtime environment.
%%
%% Starts pg (process group) which is required by the class registry.
%% Class processes are created by compiled stdlib modules' on_load.
-spec init(pid()) -> no_return().
init(Parent) ->
    %% Ensure pg is started (required by beamtalk_object_class for class registry)
    case whereis(pg) of
        undefined ->
            {ok, _Pid} = pg:start_link();
        _ ->
            ok
    end,
    
    proc_lib:init_ack(Parent, {ok, self()}),
    bootstrap_loop().

%% @private
%% Bootstrap process loop - stays alive as part of the supervision tree
bootstrap_loop() ->
    receive
        _Any -> bootstrap_loop()
    end.
