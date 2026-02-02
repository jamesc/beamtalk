%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Application callback module for the Beamtalk runtime.
-module(beamtalk_runtime_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @private
start(_StartType, _StartArgs) ->
    %% Initialize extension registry ETS tables
    beamtalk_extensions:init(),
    beamtalk_runtime_sup:start_link().

%% @private
stop(_State) ->
    ok.
