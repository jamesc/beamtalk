%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with throwing doesNotUnderstand handler

-module(test_throwing_dnu_actor).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations
-export(['handle_doesNotUnderstand:args:'/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '__class__' => 'ThrowingDnuActor',
        '__methods__' => #{
            'doesNotUnderstand:args:' => fun ?MODULE:'handle_doesNotUnderstand:args:'/2
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% DNU handler that throws
'handle_doesNotUnderstand:args:'([_Selector, _Args], _State) ->
    error(dnu_intentional_error).
