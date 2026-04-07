%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_telemetry_spike_echo_server).
-behaviour(gen_server).

-moduledoc """
Minimal gen_server for telemetry spike tests.
Supports echo and sleep operations for testing telemetry:span/3 wrapping.
""".

-export([init/1, handle_call/3, handle_cast/2]).

init([]) ->
    {ok, #{}}.

handle_call({echo, Value}, _From, State) ->
    {reply, {ok, Value}, State};
handle_call({sleep, Ms}, _From, State) ->
    timer:sleep(Ms),
    {reply, {ok, slept}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
