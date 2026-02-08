%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with new-style (Fun/3) doesNotUnderstand handler for BT-159 testing

-module(test_self_aware_proxy).
-behaviour(gen_server).

-include("beamtalk.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations (new-style DNU handler with Self parameter)
-export(['handle_doesNotUnderstand:args:'/3]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'SelfAwareProxy',
        '__class_mod__' => 'test_self_aware_proxy',
        '__methods__' => #{
            'doesNotUnderstand:args:' => fun ?MODULE:'handle_doesNotUnderstand:args:'/3
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% New-style DNU handler (Fun/3): receives ([Selector, Args], Self, State)
'handle_doesNotUnderstand:args:'([_Selector, _Args], Self, State) ->
    %% Return Self to prove we received it correctly
    {reply, Self, State}.
