%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_wirecheck_actor).
-behaviour(gen_server).

-moduledoc """
Test actor fixture for the ADR 0126 Phase 0.5 wire-check spike
(BT-3579, `beamtalk_dist_wirecheck_tests`).

Mirrors `test_counter.erl`'s shape (raw `__methods__` map, not a compiled
`.bt` class — see that module's doc and BT-3579's "if needed" note on `.bt`
fixtures) and adds the two operations the spike's block/NLR scenarios need
that `test_counter` has no reason to carry: invoking a caller-supplied block
(`'invokeBlock:'`).
""".

%% API
-export([start_link/1, start/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Method implementations
-export([
    handle_getValue/2,
    'handle_invokeBlock:'/2
]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

-doc "Start without a link — the spike drives this actor entirely via erpc/rpc from another node.".
start(InitialValue) ->
    gen_server:start(?MODULE, InitialValue, []).

init(InitialValue) ->
    test_actor_host:init(#{
        '$beamtalk_class' => 'WirecheckActor',
        '__class_mod__' => 'wirecheck_actor',
        '__methods__' => #{
            getValue => fun ?MODULE:handle_getValue/2,
            'invokeBlock:' => fun ?MODULE:'handle_invokeBlock:'/2
        },
        value => InitialValue
    }).

handle_cast(Msg, State) -> test_actor_host:handle_cast(Msg, State).
handle_call(Msg, From, State) -> test_actor_host:handle_call(Msg, From, State).
handle_info(Msg, State) -> test_actor_host:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> test_actor_host:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> test_actor_host:terminate(Reason, State).

%% Method implementations

handle_getValue([], State) ->
    {reply, maps:get(value, State), State}.

-doc """
Invoke a zero-arity block passed as the sole argument, in this actor's own
process — the scenario BT-3579 (c) needs: a block containing `^`, defined on
node A, invoked here on node B during A's sync call.

Deliberately does **not** wrap the call in its own try/catch: the point of
the spike is to observe what `beamtalk_actor:dispatch_user_method/4`'s own
catch (the one every compiled method body runs under) actually does with an
escaping `{'$bt_nlr', ...}` throw, not to paper over it here.
""".
'handle_invokeBlock:'([Block], State) when is_function(Block, 0) ->
    Result = Block(),
    {reply, Result, State}.
