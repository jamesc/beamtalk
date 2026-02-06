%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with multi-argument methods for perform:withArgs: tests

-module(test_multi_arg_actor).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations
-export(['handle_compute:plus:'/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '__class__' => 'MultiArgActor',
        '__class_mod__' => 'multi_arg_actor',
        '__methods__' => #{
            'compute:plus:' => fun ?MODULE:'handle_compute:plus:'/2
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method implementations
'handle_compute:plus:'([A, B], State) ->
    Result = A + B,
    {reply, Result, State}.
