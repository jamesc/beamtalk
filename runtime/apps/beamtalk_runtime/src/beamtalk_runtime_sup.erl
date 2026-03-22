%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Top-level supervisor for the Beamtalk runtime.
%%%
%%% **DDD Context:** Object System Context
-module(beamtalk_runtime_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%% @doc Start the runtime supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    case Result of
        {ok, Pid} ->
            ?LOG_INFO("Runtime supervisor started", #{pid => Pid, domain => [beamtalk, runtime]});
        _ ->
            ok
    end,
    Result.

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
        },
        %% Supervisor for Subprocess gen_servers (simple_one_for_one, temporary)
        #{
            id => beamtalk_subprocess_sup,
            start => {beamtalk_subprocess_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [beamtalk_subprocess_sup]
        },
        %% Supervisor for ReactiveSubprocess gen_servers (simple_one_for_one, temporary)
        #{
            id => beamtalk_reactive_subprocess_sup,
            start => {beamtalk_reactive_subprocess_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [beamtalk_reactive_subprocess_sup]
        },
        %% Trace store for actor observability (ADR 0069)
        #{
            id => beamtalk_trace_store,
            start => {beamtalk_trace_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_trace_store]
        }
    ],

    ChildIds = [maps:get(id, S) || S <- ChildSpecs],
    ?LOG_DEBUG("Runtime supervisor init", #{
        strategy => one_for_one,
        intensity => 5,
        period => 10,
        children => ChildIds,
        domain => [beamtalk, runtime]
    }),
    {ok, {SupFlags, ChildSpecs}}.
