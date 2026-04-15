%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_gen_server).

-moduledoc """
Minimal gen_server for use as a test double in EUnit tests (BT-1967).
""".

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
