%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Root supervisor for Beamtalk workspace application.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This supervisor manages workspace supervisors dynamically.
%%% Each workspace is supervised independently and can be started/stopped
%%% on demand.
%%%
%%% Currently, the CLI starts workspaces directly via beamtalk_workspace_sup:start_link/1
%%% without going through this supervisor. This is a transitional state - future
%%% work (ADR 0004) will move to dynamic workspace supervision under this tree.
%%%
%%% Architecture:
%%% ```
%%% beamtalk_workspace_app_sup (one_for_one)
%%%   └─ [per-workspace children added dynamically]
%%%       └─ beamtalk_workspace_sup (one_for_one)
%%%           ├─ beamtalk_workspace_meta
%%%           ├─ beamtalk_repl_actors
%%%           ├─ beamtalk_repl_server
%%%           ├─ beamtalk_idle_monitor
%%%           ├─ beamtalk_actor_sup
%%%           └─ beamtalk_session_sup
%%% ```

-module(beamtalk_workspace_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc Start the workspace root supervisor.
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

    %% No static children - workspaces are added dynamically
    %% (currently done by CLI directly, future ADR 0004 work will use supervisor:start_child)
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.
