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
%%%
%%% ADR 0032 Phase 0 (BT-732): Bootstrap registers the 'Class' stub
%%% (beamtalk_class_bt) to prove class chain dispatch fallthrough. This
%%% registration happens after stdlib loads so that Object is available
%%% as the superclass. See beamtalk_class_bt for the stub details.
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
%%
%% ADR 0032 Phase 0 (BT-732): Also registers the 'Class' stub so the
%% class chain dispatch fallthrough is available from startup.
-spec init(pid()) -> no_return().
init(Parent) ->
    %% Ensure pg is started (required by beamtalk_object_class for class registry)
    case whereis(pg) of
        undefined ->
            {ok, _Pid} = pg:start_link();
        _ ->
            ok
    end,

    %% ADR 0032 Phase 0 (BT-732): Register the 'Class' stub.
    %% This is safe to call before stdlib loads — the module is part of
    %% beamtalk_runtime, and dispatch works via beamtalk_class_bt:has_method/1
    %% and dispatch/4 even before the superclass (Object) is registered.
    beamtalk_class_bt:register_class(),

    proc_lib:init_ack(Parent, {ok, self()}),
    bootstrap_loop().

%% @private
%% Bootstrap process loop - stays alive as part of the supervision tree
bootstrap_loop() ->
    receive
        _Any -> bootstrap_loop()
    end.
