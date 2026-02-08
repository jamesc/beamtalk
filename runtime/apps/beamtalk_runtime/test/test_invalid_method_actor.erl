%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with invalid method (not a function)

-module(test_invalid_method_actor).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'InvalidMethodActor',
        '__class_mod__' => 'test_invalid_method_actor',
        '__methods__' => #{
            notAFunction => "this is a string, not a function",
            validMethod => fun(_Params, State) -> {reply, ok, State} end
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).
