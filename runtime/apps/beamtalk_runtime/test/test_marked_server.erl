%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_marked_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
A bare gen_server that plants the `'$beamtalk_actor'` marker for an arbitrary
class and answers every `handle_call` with `{ok, pong}` (BT-4949). Lets a test
make a bare pid look like an actor of any class — `'TimeoutProxy'` in
particular — without compiling one.
""".

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(Class) ->
    gen_server:start_link(?MODULE, Class, []).

init(Class) ->
    erlang:put('$beamtalk_actor', Class),
    {ok, Class}.

handle_call(_Request, _From, State) ->
    {reply, {ok, pong}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
