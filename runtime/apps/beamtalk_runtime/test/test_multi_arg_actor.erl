%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_multi_arg_actor).
-behaviour(gen_server).

-moduledoc "Test actor with multi-argument methods for perform:withArguments: tests".

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Method implementations
-export(['handle_compute:plus:'/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    test_actor_host:init(#{
        '$beamtalk_class' => 'MultiArgActor',
        '__class_mod__' => 'multi_arg_actor',
        '__methods__' => #{
            'compute:plus:' => fun ?MODULE:'handle_compute:plus:'/2
        }
    }).

handle_cast(Msg, State) -> test_actor_host:handle_cast(Msg, State).
handle_call(Msg, From, State) -> test_actor_host:handle_call(Msg, From, State).
handle_info(Msg, State) -> test_actor_host:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> test_actor_host:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> test_actor_host:terminate(Reason, State).

%% Method implementations
'handle_compute:plus:'([A, B], State) ->
    Result = A + B,
    {reply, Result, State}.
