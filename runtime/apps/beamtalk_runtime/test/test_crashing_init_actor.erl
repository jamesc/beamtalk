%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor that crashes during handle_continue (simulating an
%%% initialize method that raises an error). Used to verify that
%%% safe_spawn/await_initialize preserves the full stop reason.

-module(test_crashing_init_actor).
-behaviour(gen_server).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, terminate/2]).

init(_Args) ->
    %% Defer initialization to handle_continue (same pattern as generated actors)
    {ok, #{}, {continue, initialize}}.

handle_continue(initialize, _State) ->
    %% Simulate an initialize method that crashes with a tuple reason
    {stop, {error, function_clause}, #{}};
handle_continue(_, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
