%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace API for managing workspace-level bindings.
%%%
%%% **DDD Context:** Workspace
%%%
%%% Provides functions for setting workspace convenience bindings
%%% (e.g., Transcript, Beamtalk, Workspace) that resolve in REPL
%%% sessions via persistent_term.

-module(beamtalk_workspace).

-export([set_binding/2]).

%% @doc Set a workspace convenience binding.
%%
%% Makes `Name` resolve to `Value` in REPL evaluation via the
%% `persistent_term:get({beamtalk_binding, Name})` codegen path.
-spec set_binding(atom(), term()) -> ok.
set_binding(Name, Value) ->
    persistent_term:put({beamtalk_binding, Name}, Value),
    ok.
