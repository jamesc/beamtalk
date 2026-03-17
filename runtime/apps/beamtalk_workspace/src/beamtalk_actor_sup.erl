%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Actor supervisor for workspace
%%%
%%% **DDD Context:** Actor System Context
%%%
%%% Supervises all user-spawned actors in a workspace.
%%% Actors are shared across all REPL sessions in the same workspace.
%%%
%%% This supervisor uses simple_one_for_one strategy to dynamically
%%% start actor processes as users spawn them.

-module(beamtalk_actor_sup).
-behaviour(supervisor).

-export([start_link/0, start_actor/3]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%% Public API

%% @doc Start the actor supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    case Result of
        {ok, Pid} ->
            ?LOG_INFO("Actor supervisor started", #{pid => Pid});
        _ ->
            ok
    end,
    Result.

%% @doc Start a new actor under supervision.
%% Module:spawn/0 should start the actor gen_server.
-spec start_actor(module(), atom(), list()) -> {ok, pid()} | {error, term()}.
start_actor(Module, Function, Args) ->
    Result = supervisor:start_child(?MODULE, [Module, Function, Args]),
    case Result of
        {ok, Pid} ->
            ?LOG_INFO("Actor child started", #{module => Module, child_pid => Pid});
        {error, Reason} ->
            ?LOG_ERROR("Actor child start failed", #{module => Module, reason => Reason})
    end,
    Result.

%%% supervisor callbacks

%% @private
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    %% Child spec template for actors
    ChildSpec = #{
        id => beamtalk_actor,
        start => {beamtalk_actor, start_link_supervised, []},
        % Don't restart crashed actors automatically
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamtalk_actor]
    },

    {ok, {SupFlags, [ChildSpec]}}.
