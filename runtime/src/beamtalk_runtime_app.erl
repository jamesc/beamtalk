%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Application callback module for the Beamtalk runtime.
-module(beamtalk_runtime_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @private
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Initialize extension registry ETS tables
    beamtalk_extensions:init(),
    
    %% Start the runtime supervisor tree (which starts beamtalk_bootstrap and pg)
    beamtalk_runtime_sup:start_link().

%% @private
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
