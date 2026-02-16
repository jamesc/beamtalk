%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Application callback module for the Beamtalk runtime.
-module(beamtalk_runtime_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @private
%% @doc Start the Beamtalk runtime application, initializing ETS tables and supervisor tree.
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Initialize extension registry ETS tables
    beamtalk_extensions:init(),
    
    %% BT-510: Create class hierarchy ETS table at app startup so it's owned
    %% by the application master (survives individual class process crashes).
    beamtalk_class_registry:ensure_hierarchy_table(),
    
    %% Start the runtime supervisor tree (which starts beamtalk_bootstrap, beamtalk_stdlib,
    %% and beamtalk_object_instances; pg is conditionally started inside beamtalk_bootstrap:init/1)
    beamtalk_runtime_sup:start_link().

%% @private
%% @doc Stop the Beamtalk runtime application.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
