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
    case beamtalk_runtime_sup:start_link() of
        {ok, _Pid} = Ok ->
            %% Load standard library modules to trigger auto-registration.
            %% Each stdlib module (e.g., beamtalk.beam) has an -on_load attribute
            %% that calls register_class/0, which creates a class process and registers
            %% it with the beamtalk_class registry. This must happen after pg is started
            %% (which happens in beamtalk_bootstrap via the supervisor tree).
            %%
            %% Without this, calling 'Beamtalk allClasses' in the REPL would fail with
            %% error:undef because the module isn't loaded and the class isn't registered.
            case code:ensure_loaded(beamtalk) of
                {module, beamtalk} ->
                    Ok;
                {error, Reason} ->
                    logger:error("Failed to load Beamtalk stdlib module 'beamtalk': ~p", [Reason]),
                    {error, {beamtalk_stdlib_load_failed, Reason}}
            end;
        Error ->
            Error
    end.

%% @private
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
