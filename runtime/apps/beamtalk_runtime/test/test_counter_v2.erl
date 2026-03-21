%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test counter V2 actor for migration tests (BT-106).
%%%
%%% An upgraded counter with an additional `label` field.
%%% Used to test migrate: protocol — migrating from test_counter to
%%% test_counter_v2 should preserve the `value` field and add `label`.

-module(test_counter_v2).
-behaviour(gen_server).

%% API
-export([start_link/1]).

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
-export([
    handle_increment/2,
    handle_getValue/2,
    handle_getLabel/2,
    'handle_setLabel:'/2
]).

start_link(InitialValue) ->
    beamtalk_actor:start_link(?MODULE, InitialValue).

init(InitState) when is_map(InitState) ->
    %% Support both fresh init (empty map) and migration (state map with fields)
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'CounterV2',
        '__class_mod__' => 'counter_v2',
        '__methods__' => #{
            increment => fun ?MODULE:handle_increment/2,
            getValue => fun ?MODULE:handle_getValue/2,
            getLabel => fun ?MODULE:handle_getLabel/2,
            'setLabel:' => fun ?MODULE:'handle_setLabel:'/2
        },
        value => maps:get(value, InitState, 0),
        label => maps:get(label, InitState, <<"default">>)
    });
init(InitialValue) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'CounterV2',
        '__class_mod__' => 'counter_v2',
        '__methods__' => #{
            increment => fun ?MODULE:handle_increment/2,
            getValue => fun ?MODULE:handle_getValue/2,
            getLabel => fun ?MODULE:handle_getLabel/2,
            'setLabel:' => fun ?MODULE:'handle_setLabel:'/2
        },
        value => InitialValue,
        label => <<"default">>
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method implementations
handle_increment([], State) ->
    Value = maps:get(value, State),
    NewState = maps:put(value, Value + 1, State),
    {noreply, NewState}.

handle_getValue([], State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.

handle_getLabel([], State) ->
    Label = maps:get(label, State),
    {reply, Label, State}.

'handle_setLabel:'([NewLabel], State) ->
    NewState = maps:put(label, NewLabel, State),
    {reply, ok, NewState}.
