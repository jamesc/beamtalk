%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_slow_spawn_test_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
BT-5021: implementation module for a test actor class whose `initialize`
outlasts the 5 s init timeout, so `class_send(Class, spawn, [])` reaches the
class process's own structured `timeout` reply.
""".

-export([spawn/0, spawn/1]).

-spec spawn() -> {ok, pid()} | {error, term()}.
spawn() ->
    beamtalk_actor:safe_spawn(test_init_ack_actor, #{mode => slow_initialize, delay => 7000}).

-spec spawn(map()) -> {ok, pid()} | {error, term()}.
spawn(_Args) ->
    ?MODULE:spawn().
