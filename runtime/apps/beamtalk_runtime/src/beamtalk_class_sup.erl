%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_sup).
-behaviour(supervisor).

%%% **DDD Context:** Object System Context

-moduledoc """
simple_one_for_one supervisor for class gen_server processes (BT-3236).

Every `beamtalk_object_class` process is started through this supervisor via
`start_child/2`, placing class processes in the standard OTP supervision
tree (visible in `observer` / `SystemNavigation supervisionTree`) instead of
the previous unlinked `gen_server:start/4`.

Children are `temporary`: OTP's own restart machinery is deliberately NOT
used, because a supervisor restart would replay the original `ClassInfo`
(stale after hot reloads) and a crash-looping class would escalate through
restart intensity and take down every other class under this supervisor.
Instead, eager crash recovery is owned by `beamtalk_class_monitor`, which
rebuilds fresh state from ETS + `__beamtalk_meta/0` via
`beamtalk_class_registry:restart_class/1` and applies a per-class restart
budget.

This supervisor is started by `beamtalk_runtime_sup` (before
`beamtalk_bootstrap`, so the bootstrap stub classes register through it)
and registered locally as `beamtalk_class_sup`.
""".

-export([start_link/0, start_child/2]).
-export([init/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the supervisor (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Start a supervised class gen_server for `ClassName`.

Returns `{ok, Pid}`, or `{error, {already_started, Pid}}` when the class's
registered name is already taken — the same contract as
`beamtalk_object_class:start/2`, whose callers all handle that tuple.
""".
-spec start_child(atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(ClassName, ClassInfo) ->
    supervisor:start_child(?MODULE, [ClassName, ClassInfo]).

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
        id => beamtalk_object_class,
        start => {beamtalk_object_class, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamtalk_object_class]
    },
    {ok, {SupFlags, [ChildSpec]}}.
