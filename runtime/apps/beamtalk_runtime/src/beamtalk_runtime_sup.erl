%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Top-level supervisor for the Beamtalk runtime.
%%%
%%% **DDD Context:** Runtime
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
        %% Bootstrap the class hierarchy first
        #{
            id => beamtalk_bootstrap,
            start => {beamtalk_bootstrap, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_bootstrap]
        },
        %% Then register stdlib primitive classes (Integer, String, etc.)
        #{
            id => beamtalk_stdlib,
            start => {beamtalk_stdlib, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_stdlib]
        },
        %% Then start instance tracking
        #{
            id => beamtalk_object_instances,
            start => {beamtalk_object_instances, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_object_instances]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
