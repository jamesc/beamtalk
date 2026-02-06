%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test counter actor for beamtalk_actor tests

-module(test_counter).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations
-export([handle_increment/2, handle_decrement/2, handle_getValue/2,
         'handle_setValue:'/2, handle_test_make_self/2]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

init(InitialValue) ->
    beamtalk_actor:init(#{
        '__class__' => 'Counter',
        '__class_mod__' => 'counter',
        '__methods__' => #{
            increment => fun ?MODULE:handle_increment/2,
            decrement => fun ?MODULE:handle_decrement/2,
            getValue => fun ?MODULE:handle_getValue/2,
            'setValue:' => fun ?MODULE:'handle_setValue:'/2,
            test_make_self => fun ?MODULE:handle_test_make_self/2
        },
        value => InitialValue
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method implementations
handle_increment([], State) ->
    %% Note: Returns {noreply, NewState} - no return value
    %% When called sync, will receive 'nil' as reply
    Value = maps:get(value, State),
    NewValue = Value + 1,
    NewState = maps:put(value, NewValue, State),
    {noreply, NewState}.

handle_decrement([], State) ->
    Value = maps:get(value, State),
    NewValue = Value - 1,
    NewState = maps:put(value, NewValue, State),
    {noreply, NewState}.

handle_getValue([], State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.

'handle_setValue:'([NewValue], State) ->
    NewState = maps:put(value, NewValue, State),
    {reply, ok, NewState}.

handle_test_make_self([], State) ->
    %% Test helper to expose make_self/1
    Self = beamtalk_actor:make_self(State),
    {reply, Self, State}.
