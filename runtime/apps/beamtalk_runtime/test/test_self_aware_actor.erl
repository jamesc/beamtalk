%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with new-style methods (Fun/4) for BT-159 testing

-module(test_self_aware_actor).
-behaviour(gen_server).

-include("beamtalk.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations (new-style with Self parameter)
-export([handle_getSelf/4, handle_getClassName/4]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

init(InitialValue) ->
    beamtalk_actor:init(#{
        '__class__' => 'SelfAwareActor',
        '__class_mod__' => 'test_self_aware_actor',
        '__methods__' => #{
            getSelf => fun ?MODULE:handle_getSelf/4,
            getClassName => fun ?MODULE:handle_getClassName/4
        },
        value => InitialValue
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method implementations (new-style: receive Selector, Args, Self, State)
handle_getSelf(_Selector, _Args, Self, State) ->
    %% Return the Self reference
    {reply, Self, State}.

handle_getClassName(_Selector, _Args, Self, State) ->
    %% Extract class from Self
    Class = Self#beamtalk_object.class,
    {reply, Class, State}.
