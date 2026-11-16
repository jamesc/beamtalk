%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_proxy).
-behaviour(gen_server).

-moduledoc "Test proxy actor with doesNotUnderstand handler".

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
-export(['handle_doesNotUnderstand:args:'/2, handle_setTarget/2]).

start_link(Target) ->
    beamtalk_actor:start_link(?MODULE, Target).

init(Target) ->
    test_actor_host:init(#{
        '$beamtalk_class' => 'Proxy',
        '__class_mod__' => 'test_proxy',
        '__methods__' => #{
            'doesNotUnderstand:args:' => fun ?MODULE:'handle_doesNotUnderstand:args:'/2,
            setTarget => fun ?MODULE:handle_setTarget/2
        },
        target => Target
    }).

handle_cast(Msg, State) -> test_actor_host:handle_cast(Msg, State).
handle_call(Msg, From, State) -> test_actor_host:handle_call(Msg, From, State).
handle_info(Msg, State) -> test_actor_host:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> test_actor_host:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> test_actor_host:terminate(Reason, State).

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
