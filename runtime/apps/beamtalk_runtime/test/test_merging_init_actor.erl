%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_merging_init_actor).
-behaviour(gen_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-3992: gen_server whose `init/1` merges its start args into its state and
defers to `{continue, initialize}` without consuming the initialize-ack
target — the shape of an actor module compiled before BT-3992. Used to
check that the sys-based fallback still starts it and strips the target
from its state.
""".

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2]).

init(Args) when is_map(Args) ->
    {ok, maps:merge(#{value => 0}, Args), {continue, initialize}}.

handle_continue(initialize, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
