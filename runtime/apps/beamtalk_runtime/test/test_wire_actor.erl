%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_wire_actor).
-behaviour(gen_server).

-moduledoc """
Test actor fixture for the ADR 0126 Phase 3b wire integration suite
(BT-3601, `beamtalk_dist_wire_tests`).

Mirrors `test_wirecheck_actor.erl`'s shape (a raw `__methods__` map, not a
compiled `.bt` class — see that module's doc) and adds the operations
BT-3601's own scenarios need that the Phase 0.5 spike fixture has no reason
to carry: an arg/result round-trip (`echo:`), a `late`-slot stand-in
(`setLate:`/`lateOp`, raising the same `uninitialized_state_error` a real
guarded `late` read raises when unset), a hand-built too-new envelope
returned as a reply (`shapeAheadReply`), and a raise via
`beamtalk_error:raise/1` — the "double-wrap" path `handle_boom`
(`test_wirecheck_actor`) deliberately does not exercise.
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
    'handle_echo:'/2,
    'handle_setLate:'/2,
    handle_lateOp/2,
    handle_shapeAheadReply/2,
    handle_boomWrapped/2,
    'handle_invokeBlockWith:'/2
]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

-doc "Start without a link — driven entirely via cross-node sends from another node, like test_wirecheck_actor:start/1.".
start(InitialValue) ->
    gen_server:start(?MODULE, InitialValue, []).

init(InitialValue) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'WireActor',
        '__class_mod__' => 'wire_actor',
        '__methods__' => #{
            getValue => fun ?MODULE:handle_getValue/2,
            'echo:' => fun ?MODULE:'handle_echo:'/2,
            'setLate:' => fun ?MODULE:'handle_setLate:'/2,
            lateOp => fun ?MODULE:handle_lateOp/2,
            shapeAheadReply => fun ?MODULE:handle_shapeAheadReply/2,
            boomWrapped => fun ?MODULE:handle_boomWrapped/2,
            'invokeBlockWith:' => fun ?MODULE:'handle_invokeBlockWith:'/2
        },
        value => InitialValue
        %% Deliberately no 'late_field' key here — the `late`-slot stand-in
        %% (see lateOp/2's doc) starts genuinely absent, matching ADR 0124's
        %% "no sentinel, absent means absent" contract.
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method implementations

handle_getValue([], State) ->
    {reply, maps:get(value, State), State}.

-doc "Round-trips its single argument unchanged — used to exercise the wire's arg-decode / reply-encode paths without any other side effect.".
'handle_echo:'([Value], State) ->
    {reply, Value, State}.

-doc "Assigns the `late_field` stand-in, mirroring a real `late` slot's first write.".
'handle_setLate:'([Value], State) ->
    {reply, Value, maps:put(late_field, Value, State)}.

-doc """
Reads the `late_field` stand-in. Absent (never assigned) raises the same
`uninitialized_state_error` a real guarded `late` read raises
(`beamtalk_reflection:raise_uninitialized_state/2`'s error shape,
reconstructed here since that function is not exported — this fixture is
not a compiled `.bt` class, so it cannot go through the real codegen'd
guarded-read path, only reproduce its error kind and shape for the test).
""".
handle_lateOp([], State) ->
    case maps:find(late_field, State) of
        {ok, Value} ->
            {reply, Value, State};
        error ->
            Error = beamtalk_error:with_hint(
                beamtalk_error:new(uninitialized_state_error, 'WireActor', lateOp),
                <<"'late_field' was read before being assigned">>
            ),
            {error, Error, State}
    end.

-doc """
Returns a hand-built envelope one shape version ahead of `ShapeChainCart`'s
real (unmocked) known version (1) — drives the reply-direction
`shape_version_ahead` scenario without needing any node-specific meta
override, since the *decoding* node's own always-real knowledge of
`ShapeChainCart` (version 1) is what the check compares against, on
whichever node calls `beamtalk_wire:decode/1` for the reply.
""".
handle_shapeAheadReply([], State) ->
    Envelope =
        {beamtalk_shape, 'ShapeChainCart', 999, #{
            itemCount => 1, total => 0, tag => <<"none">>
        }},
    {reply, Envelope, State}.

-doc """
Raises via `beamtalk_error:raise/1` (not a plain `error/1`) — the
"double-wrap" case ADR 0126's Amendment (d) calls out: `raise/1` wraps the
`#beamtalk_error{}` as an Exception tagged map itself, so
`dispatch_user_method/4`'s first catch clause (which matches a *bare*
`#beamtalk_error{}`) does not fire, and the reason a future rejects with is
a fresh `runtime_error` whose `details.original_reason` holds the
already-wrapped map — contrast `test_wirecheck_actor:handle_boom/2`, which
raises the bare record directly.
""".
handle_boomWrapped([], _State) ->
    beamtalk_error:raise(
        beamtalk_error:new(runtime_error, 'WireActor', boomWrapped, <<"boom, wrapped">>)
    ).

-doc """
Invoke a one-argument block passed as the sole argument, in this actor's own
process — the `remote_code_mismatch` scenarios (ADR 0126 §5.5, BT-3579
finding (b)) need a block shaped like
`beamtalk_dist_wirecheck_tests:compile_block_module_variant/2`'s generated
`fun(X) -> ... end` (arity 1), unlike `test_wirecheck_actor`'s zero-arity
`'invokeBlock:'` (built for the NLR scenario's `fun() -> throw(...) end`).
""".
'handle_invokeBlockWith:'([Block], State) when is_function(Block, 1) ->
    Result = Block(1),
    {reply, Result, State}.
