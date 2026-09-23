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
(`'invokeBlock:'`) and raising a structured error on demand (`boom`, for the
async-future rejection case).
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
    'handle_invokeBlock:'/2,
    handle_boom/2
]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

-doc "Start without a link — the spike drives this actor entirely via erpc/rpc from another node.".
start(InitialValue) ->
    gen_server:start(?MODULE, InitialValue, []).

init(InitialValue) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'WirecheckActor',
        '__class_mod__' => 'wirecheck_actor',
        '__methods__' => #{
            getValue => fun ?MODULE:handle_getValue/2,
            'invokeBlock:' => fun ?MODULE:'handle_invokeBlock:'/2,
            boom => fun ?MODULE:handle_boom/2
        },
        value => InitialValue
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

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

-doc """
Raise a structured error unconditionally — drives the async-future
rejection case (BT-3579 (d)).

Raises the bare `#beamtalk_error{}` record with a plain `error/1`, matching
`dispatch_user_method/4`'s *first* catch clause (`error:#beamtalk_error{} =
BtError -> {error, BtError, State}`) — not `beamtalk_error:raise/1`, whose
`beamtalk_exception_handler:wrap/1` step is for errors escaping to a
top-level caller (REPL/CLI) that expects the `'$beamtalk_class'`-tagged map
shape, and would double-wrap here since dispatch already performs its own
wrapping before this error reaches `sync_send`/`async_send`'s caller.
""".
handle_boom([], _State) ->
    error(beamtalk_error:new(runtime_error, 'WirecheckActor', boom, <<"boom">>)).
