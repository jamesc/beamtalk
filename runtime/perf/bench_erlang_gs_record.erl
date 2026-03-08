%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Idiomatic Erlang gen_server counter using a record for state.
%%%
%%% Baseline for overhead comparison: this is how you would write a counter
%%% in hand-written Erlang — record state, direct pattern matching in handle_call,
%%% no dispatch indirection. This is as fast as a gen_server counter can get.

-module(bench_erlang_gs_record).
-behaviour(gen_server).

-export([start_link/0, increment/1, get_value/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {value = 0 :: integer()}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

increment(Pid) ->
    gen_server:call(Pid, increment).

get_value(Pid) ->
    gen_server:call(Pid, get_value).

init([]) ->
    {ok, #state{}}.

handle_call(increment, _From, #state{value = V} = S) ->
    {reply, ok, S#state{value = V + 1}};
handle_call(get_value, _From, #state{value = V} = S) ->
    {reply, V, S};
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
