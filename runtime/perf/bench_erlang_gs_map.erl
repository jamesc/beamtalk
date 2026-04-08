%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime benchmark — gen_server counter with map state
%%%

-module(bench_erlang_gs_map).
-behaviour(gen_server).

-moduledoc """
Erlang gen_server counter using a map for state (no dispatch layer).

Baseline for overhead comparison: same map-based state as Beamtalk actors,
but with direct pattern matching in handle_call — no method table lookup,
no make_self, no dispatch indirection. Isolates the cost of map state vs
record state from the cost of Beamtalk's dispatch machinery.
""".

-export([start_link/0, increment/1, get_value/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

increment(Pid) ->
    gen_server:call(Pid, increment).

get_value(Pid) ->
    gen_server:call(Pid, get_value).

init([]) ->
    {ok, #{value => 0}}.

handle_call(increment, _From, #{value := V} = State) ->
    {reply, ok, State#{value => V + 1}};
handle_call(get_value, _From, #{value := V} = State) ->
    {reply, V, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
