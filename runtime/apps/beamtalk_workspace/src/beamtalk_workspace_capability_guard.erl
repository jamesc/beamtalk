%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_capability_guard).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Keeps this node's recorded capabilities (`beamtalk_capability`) in step with
the life of `beamtalk_workspace_sup` (ADR 0129 §4).

The capabilities live in a `persistent_term`, which outlives the process that
wrote it. `beamtalk_workspace_sup` is a plain `supervisor` and has no
terminate hook, so it starts this trivial worker as its **first** child:
children stop in reverse start order, so it stops last, and its `terminate/2`
clears the capabilities once the rest of the workspace is down. A stopped
workspace therefore no longer satisfies `beamtalk_capability:require_workspace/1`.

The worker traps exits so `terminate/2` runs on a supervisor shutdown. On a
restart it re-records the capabilities it was given.
""".

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-doc "Start the guard, recording `Capabilities` for the life of the process.".
-spec start_link(beamtalk_capability:capabilities()) -> {ok, pid()} | {error, term()}.
start_link(Capabilities) ->
    gen_server:start_link(?MODULE, Capabilities, []).

-spec init(beamtalk_capability:capabilities()) -> {ok, beamtalk_capability:capabilities()}.
init(Capabilities) ->
    process_flag(trap_exit, true),
    ok = beamtalk_capability:set(Capabilities),
    {ok, Capabilities}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), beamtalk_capability:capabilities()) -> ok.
terminate(_Reason, _State) ->
    beamtalk_capability:clear().
