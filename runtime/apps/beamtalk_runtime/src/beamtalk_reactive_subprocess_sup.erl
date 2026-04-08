%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_reactive_subprocess_sup).
-behaviour(supervisor).

%%% **DDD Context:** Actor System Context

-moduledoc """
simple_one_for_one supervisor for beamtalk_reactive_subprocess gen_servers (BT-1187).

Each ReactiveSubprocess actor starts one `beamtalk_reactive_subprocess`
gen_server via `start_child/1`.  The `temporary` restart strategy means a
crashed subprocess is never automatically restarted — the notify actor
receives a `subprocessExit:from:` cast when the process terminates.

This supervisor is started by `beamtalk_runtime_sup` and registered locally
as `beamtalk_reactive_subprocess_sup`.
""".

-export([start_link/0, start_child/1]).
-export([init/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the supervisor (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Start a supervised beamtalk_reactive_subprocess gen_server with the given Config.

Config must contain `executable` (binary) and `notify` (#beamtalk_object{}).
Optional keys: `args`, `env`, `dir`.
Returns `{ok, Pid}` on success or `{error, Reason}` on failure.
""".
-spec start_child(map()) -> {ok, pid()} | {error, term()}.
start_child(Config) ->
    supervisor:start_child(?MODULE, [Config]).

%%% ============================================================================
%%% supervisor callback
%%% ============================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpec = #{
        id => beamtalk_reactive_subprocess,
        start => {beamtalk_reactive_subprocess, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamtalk_reactive_subprocess]
    },
    {ok, {SupFlags, [ChildSpec]}}.
