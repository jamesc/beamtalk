%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app_sup).
-behaviour(supervisor).

%%% **DDD Context:** Workspace Context

-moduledoc """
Root supervisor for Beamtalk workspace application.

This supervisor manages workspace supervisors dynamically.
Each workspace is supervised independently and can be started/stopped
on demand.

The CLI's `run`/`workspace`/escript paths still start
`beamtalk_workspace_sup` directly via an `-eval` string, entirely outside
this tree — a transitional state future work (ADR 0004) may replace. A
release, however, sets `mode` in the `beamtalk_workspace` application env
(`sys.config`), and `beamtalk_workspace_app:start/2` starts a single
workspace under this supervisor via `start_workspace/1` (ADR 0125 §1.4) —
so `init/1` still returns no *static* children, but `start_workspace/1`
adds one dynamically the way ADR 0004's future work eventually generalises
to many.

Architecture:
```
beamtalk_workspace_app_sup (one_for_one)
  └─ [per-workspace children added dynamically]
      └─ beamtalk_workspace_sup (one_for_one)
          ├─ beamtalk_workspace_meta
          ├─ beamtalk_repl_actors
          ├─ beamtalk_repl_server
          ├─ beamtalk_idle_monitor
          ├─ beamtalk_actor_sup
          └─ beamtalk_session_sup
```
""".

-export([start_link/0, start_workspace/1]).
-export([init/1]).

-doc "Start the workspace root supervisor.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Start `beamtalk_workspace_sup` as a dynamic child of this supervisor
(ADR 0125 §1.4). Called only by `beamtalk_workspace_app:start/2`, and only
when the `beamtalk_workspace` application env declares a `mode` — the CLI's
run/workspace/escript paths keep starting `beamtalk_workspace_sup` directly,
outside this tree, so they never reach this function.
""".
-spec start_workspace(beamtalk_workspace_sup:workspace_config()) ->
    {ok, pid()} | {error, term()}.
start_workspace(Config) ->
    ChildSpec = #{
        id => beamtalk_workspace_sup,
        start => {beamtalk_workspace_sup, start_link, [Config]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamtalk_workspace_sup]
    },
    supervisor:start_child(?MODULE, ChildSpec).

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
