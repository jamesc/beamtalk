%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Test backing gen_server for native: facade e2e tests (BT-1210).
%%
%% A minimal counter gen_server used by NativeCounter.bt to validate
%% that dispatch functions correctly route through beamtalk_actor:sync_send/3.
-module(beamtalk_test_native_counter).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-include_lib("kernel/include/logger.hrl").

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    InitialValue = maps:get(initial, Config, 0),
    {ok, #{value => InitialValue}}.

handle_call({increment, []}, _From, #{value := V} = State) ->
    NewV = V + 1,
    {reply, {ok, NewV}, State#{value := NewV}};
handle_call({'getValue', []}, _From, #{value := V} = State) ->
    {reply, {ok, V}, State};
handle_call({'add:', [N]}, _From, #{value := V} = State) ->
    NewV = V + N,
    {reply, {ok, NewV}, State#{value := NewV}};
handle_call({'failWith:', [Reason]}, _From, State) ->
    {reply, {error, Reason}, State};
handle_call({Selector, _Args}, _From, State) ->
    ?LOG_ERROR("Unknown selector: ~p", [Selector], #{domain => [beamtalk, runtime]}),
    {reply, {error, {unknown_selector, Selector}}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
