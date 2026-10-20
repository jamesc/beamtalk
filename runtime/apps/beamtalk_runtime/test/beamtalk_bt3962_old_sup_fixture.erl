%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_bt3962_old_sup_fixture).
-moduledoc """
Stand-in for a supervisor module compiled before ADR 0135 (BT-3962): it
exports only `start_link/0`, which registers it under its module name and
ignores start args. `beamtalk_supervisor:start_supervisor_child/2,3` must
fall back to it rather than fail with `undef` (ADR 0125).
""".

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, []}}.
