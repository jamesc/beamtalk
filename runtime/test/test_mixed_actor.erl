%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor with mixed old-style (Fun/2) and new-style (Fun/4) methods for BT-159 testing

-module(test_mixed_actor).
-behaviour(gen_server).

-include("beamtalk.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations
-export([handle_getValueOldStyle/2, handle_getValueNewStyle/4, handle_getSelf/4]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

init(InitialValue) ->
    beamtalk_actor:init(#{
        '__class__' => 'MixedActor',
        '__class_mod__' => 'test_mixed_actor',
        '__methods__' => #{
            getValueOldStyle => fun ?MODULE:handle_getValueOldStyle/2,
            getValueNewStyle => fun ?MODULE:handle_getValueNewStyle/4,
            getSelf => fun ?MODULE:handle_getSelf/4
        },
        value => InitialValue
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Old-style method (Fun/2)
handle_getValueOldStyle([], State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.

%% New-style method (Fun/4)
handle_getValueNewStyle(_Selector, _Args, _Self, State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.

%% New-style method that uses Self
handle_getSelf(_Selector, _Args, Self, State) ->
    {reply, Self, State}.
