%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Top-level supervisor for the Beamtalk runtime.
-module(beamtalk_runtime_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc Start the runtime supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    ChildSpecs = [
        #{
            id => beamtalk_classes,
            start => {beamtalk_classes, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_classes]
        },
        #{
            id => beamtalk_instances,
            start => {beamtalk_instances, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_instances]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
