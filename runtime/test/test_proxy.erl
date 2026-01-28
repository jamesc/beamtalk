%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test proxy actor with doesNotUnderstand handler

-module(test_proxy).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).

%% Method implementations
-export(['handle_doesNotUnderstand:args:'/2, handle_setTarget/2]).

start_link(Target) ->
    beamtalk_actor:start_link(?MODULE, Target).

init(Target) ->
    beamtalk_actor:init(#{
        '__class__' => 'Proxy',
        '__methods__' => #{
            'doesNotUnderstand:args:' => fun ?MODULE:'handle_doesNotUnderstand:args:'/2,
            setTarget => fun ?MODULE:handle_setTarget/2
        },
        target => Target
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% DNU handler - forward to target
'handle_doesNotUnderstand:args:'([Selector, Args], State) ->
    Target = maps:get(target, State),
    case Target of
        nil ->
            {error, no_target, State};
        _ ->
            %% Forward as sync call
            Result = gen_server:call(Target, {Selector, Args}),
            {reply, Result, State}
    end.

handle_setTarget([NewTarget], State) ->
    NewState = maps:put(target, NewTarget, State),
    {reply, ok, NewState}.
