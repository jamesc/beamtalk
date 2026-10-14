%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_stall_after_miss_class).
-behaviour(gen_server).

-moduledoc """
Stand-in class gen_server for BT-3820 regression tests.

Answers the first `class_method_call` with `{error, not_found}` (the miss that
sends `beamtalk_class_dispatch:class_send_dispatch/3` into its fallthrough
branch) and then stalls: it stops serving its mailbox until it receives
`release`. Any further `gen_server:call` into it (`class_name`,
`module_name`) queues and, with the default 5s timeout, exits. The fixed
fallthrough resolves name and module from ETS and never messages it again.
""".

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start(?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call({class_method_call, _Selector, _Args, _Ctx}, From, State) ->
    gen_server:reply(From, {error, not_found}),
    receive
        release -> ok
    end,
    {noreply, State};
handle_call(class_name, _From, State) ->
    {reply, 'StallAfterMissClass', State};
handle_call(module_name, _From, State) ->
    {reply, test_stall_after_miss_class, State};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
