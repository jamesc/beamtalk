%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Supervisor for the Beamtalk compiler application (ADR 0022, Phase 1).
%%
%%% **DDD Context:** Compilation (Anti-Corruption Layer)
%%
%% Supervises the compiler server (which owns the OTP Port to the Rust binary).
%% Uses `one_for_one' strategy — if the server crashes, restart it (and the port).

-module(beamtalk_compiler_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    Children = [
        #{
            id => beamtalk_compiler_server,
            start => {beamtalk_compiler_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_compiler_server]
        }
    ],
    {ok, {SupFlags, Children}}.
