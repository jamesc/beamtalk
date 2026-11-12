%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_support_dummy_server).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc "Minimal gen_server fixture for the shared-helper self-tests.".

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

init(_) -> {ok, #{}}.
handle_call(_, _, S) -> {reply, ok, S}.
handle_cast(_, S) -> {noreply, S}.
