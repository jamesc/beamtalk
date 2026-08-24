%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_class_actor).
-behaviour(gen_server).

-moduledoc """
BT-3243: Minimal actor fixture wired the way generated `spawn/0` class
methods are (`beamtalk_actor:safe_spawn/2`), for tests that need to spawn a
real actor *through* a class gen_server's `{spawn, _}` handler
(`beamtalk_class_instantiation:handle_spawn/4` does
`erlang:apply(Module, spawn, Args)` — this module is that `Module`).

Distinct from `test_counter` (which only exposes `start_link/1` /
`start/1`, not `spawn/0`) so tests exercising the real class-spawn path
don't have to reuse the shared `Counter` class fixture, which other test
suites keep registered for their own duration.
""".

%% API
-export([spawn/0, spawn/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-doc "Mirrors generated `spawn/0`: beamtalk_actor:safe_spawn/2, unlinked (BT-3243).".
spawn() ->
    beamtalk_actor:safe_spawn(?MODULE, #{}).

-doc "Mirrors generated `spawn/1` (spawnWith:).".
spawn(InitArgs) when is_map(InitArgs) ->
    beamtalk_actor:safe_spawn(?MODULE, InitArgs).

init(InitArgs) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'TestClassActor',
        '__class_mod__' => ?MODULE,
        '__methods__' => #{
            getValue => fun handle_getValue/2
        },
        value => maps:get(value, InitArgs, 0)
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

handle_getValue([], State) ->
    {reply, maps:get(value, State), State}.
