%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_wirecheck_fakeclass).
-behaviour(gen_server).

-moduledoc """
Minimal stand-in "class" gen_server for the ADR 0126 Phase 0.5 wire-check
spike (BT-3579 (e), `beamtalk_dist_wirecheck_tests`).

A real class gen_server (`beamtalk_object_class.erl`) answers `module_name`
and `class_name` — see `beamtalk_behaviour_intrinsics:atom_to_class_object/1`,
which the spike calls directly rather than standing up a full compiled `.bt`
class (and the real class-instantiation/registration machinery) on two peer
nodes just to prove one rewrite. It also answers a `demo_call` selector
whose reply names the node it ran on, so the spike test can tell which
node's process actually handled a "class-side send" after the by-name
rewrite BT-3579 (e) prototypes.
""".

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-doc """
Unlinked start (`gen_server:start/3`, not `start_link/3`) — this is driven
across nodes via `rpc:call/4`, whose own temporary worker process on the
target node would otherwise link-then-vanish exactly like the unfixed
negative case in BT-3579 (a); mirrors `test_counter:start/1`'s same
unlinked-for-cross-node-driving convention.
""".
start(ClassName) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    gen_server:start({local, RegName}, ?MODULE, ClassName, []).

init(ClassName) ->
    {ok, ClassName}.

handle_call(module_name, _From, ClassName) ->
    {reply, ?MODULE, ClassName};
handle_call(class_name, _From, ClassName) ->
    {reply, ClassName, ClassName};
handle_call({demo_call, _Args}, _From, ClassName) ->
    {reply, {node(), self()}, ClassName};
handle_call(_Other, _From, ClassName) ->
    {reply, {error, unknown_call}, ClassName}.

handle_cast(_Msg, ClassName) ->
    {noreply, ClassName}.
